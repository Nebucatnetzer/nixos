;; -*- lexical-binding: t; -*-
(when (bound-and-true-p enable-clocking)
  ;; Defined outside the after-load block: they only call org when run,
  ;; and az_org_gitlab.el relies on start-main-clock existing.
  (defun start-heading-clock (id file)
    "Start clock programmatically for heading with ID in FILE."
    (require 'org-id)
    (if-let (marker (org-id-find-id-in-file id file t))
        (save-current-buffer
          (save-excursion
            (set-buffer (marker-buffer marker))
            (goto-char (marker-position marker))
            (org-clock-in)))
      (warn "Clock not started (Could not find ID '%s' in file '%s')" id file)))

  (defun start-main-clock ()
    "This functions always clocks in to the * Clock heading"
    (interactive)
    (start-heading-clock "f4294c36-0b69-4a9e-a5d9-54c924011bf0" az-org-work-file))

  ;; org-clock-in and org-clock-out are autoloaded, so these work before org loads.
  (keymap-global-set "<f6>" #'start-main-clock)
  (keymap-global-set "<f7>" #'org-clock-in)
  (keymap-global-set "<f8>" #'org-clock-out)

  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)

    (setopt org-clock-out-remove-zero-time-clocks t
            org-clock-out-when-done t
            org-clock-persist t
            ;; Do not prompt to resume an active clock
            org-clock-persist-query-resume nil)

    (setopt org-duration-format (quote (("h") (special . 2)))
            org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 4 :tcolumns 3))
            org-clocktable-defaults '(:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil
                                                :tend nil :step nil :stepskip0 nil :fileskip0 t :tags nil :match nil
                                                :emphasize nil :link nil :narrow 40! :indent t :filetitle nil
                                                :hidefiles t :formula nil :timestamp nil :level nil :tcolumns nil
                                                :formatter nil))

    (defun az-org-cc-update-clocktable ()
      "Update clocktable when C-c C-c is pressed anywhere inside one."
      (when (org-in-clocktable-p)
        (org-clock-report)
        t))

    (add-hook 'org-ctrl-c-ctrl-c-hook #'az-org-cc-update-clocktable)

    (defvar az-org-cost-center-property "COST_CENTER"
      "Property that assigns a heading and its children to an ERP cost center.")

    (defvar az-org-no-cost-center "No cost center"
      "Group name for clocked time without a cost center.")

    (defun az-org-clocktable--group-by-cost-center (tables)
      "Merge clocktable TABLES into one table grouped by cost center.
TABLES is a list of tables as `org-clock-get-table-data' returns them.
Each cost center becomes a level 1 row with its sum, followed by its rows."
      (let ((groups nil)
            (total 0))
        (dolist (table tables)
          (setq total (+ total (or (nth 1 table) 0)))
          ;; Each element is (LEVEL GROUP ROOT-LEVEL ROW) for an open ancestor.
          (let ((ancestors nil))
            (dolist (entry (nth 2 table))
              (let ((level (nth 0 entry))
                    (group (or (cdr (assoc az-org-cost-center-property (nth 5 entry)))
                               az-org-no-cost-center)))
                (while (and ancestors (>= (nth 0 (car ancestors)) level))
                  (pop ancestors))
                (let* ((parent (car ancestors))
                       (root-p (not (and parent (equal (nth 1 parent) group))))
                       (root-level (if root-p level (nth 2 parent)))
                       (row (copy-sequence entry))
                       (group-cell (assoc group groups)))
                  ;; Parent rows include this time, so remove it from the parent's group.
                  (when (and root-p parent)
                    (let ((parent-group (nth 1 parent))
                          (remaining ancestors))
                      (while (and remaining
                                  (equal (nth 1 (car remaining)) parent-group))
                        (let ((ancestor-row (nth 3 (pop remaining))))
                          (setf (nth 4 ancestor-row)
                                (- (nth 4 ancestor-row) (nth 4 entry)))))))
                  (setf (nth 0 row) (+ 2 (- level root-level)))
                  (unless group-cell
                    (setq group-cell (list group nil nil))
                    (push group-cell groups))
                  ;; group-cell is (GROUP ROWS ROOTS), both lists in reverse order.
                  (push row (nth 1 group-cell))
                  (when root-p
                    (push row (nth 2 group-cell)))
                  (push (list level group root-level row) ancestors))))))
        (let ((sorted (sort groups
                            (lambda (left right)
                              (cond ((equal (car left) az-org-no-cost-center) nil)
                                    ((equal (car right) az-org-no-cost-center) t)
                                    (t (string< (car left) (car right)))))))
              (entries nil))
          (dolist (group-cell sorted)
            (let ((sum 0))
              (dolist (root (nth 2 group-cell))
                (setq sum (+ sum (nth 4 root))))
              (push (list 1 (car group-cell) nil nil sum nil) entries)
              (dolist (row (reverse (nth 1 group-cell)))
                (push row entries))))
          (list nil total (nreverse entries)))))

    (defun az-org-clocktable-by-cost-center (ipos tables params)
      "Clocktable formatter that groups rows by `az-org-cost-center-property'.
The clocktable needs :properties (\"COST_CENTER\") :inherit-props t."
      (org-clocktable-write-default
       ipos
       (list (az-org-clocktable--group-by-cost-center tables))
       ;; The group row adds one level, so allow one more time column.
       (org-combine-plists params
                           (list :properties nil
                                 :multifile nil
                                 :maxlevel (1+ (or (plist-get params :maxlevel) 3))))))

    ;; Clocking keybindings
    (keymap-global-set "C-x C-d" #'org-clock-mark-default-task)))

;; -*- lexical-binding: t; -*-
(when (bound-and-true-p enable-clocking)
  ;; Defined outside the after-load block: they only call org when run,
  ;; and az_org_gitlab.el relies on az-start-main-clock existing.
  (defun az-start-heading-clock (id file)
    "Start clock programmatically for heading with ID in FILE."
    (require 'org-id)
    (if-let (marker (org-id-find-id-in-file id file t))
        (save-current-buffer
          (save-excursion
            (set-buffer (marker-buffer marker))
            (goto-char (marker-position marker))
            (org-clock-in)))
      (warn "Clock not started (Could not find ID '%s' in file '%s')" id file)))

  (defun az-start-main-clock ()
    "This functions always clocks in to the * Clock heading"
    (interactive)
    (az-start-heading-clock "f4294c36-0b69-4a9e-a5d9-54c924011bf0" az-org-work-file))

  ;; org-clock-in and org-clock-out are autoloaded, so these work before org loads.
  (keymap-global-set "<f6>" #'az-start-main-clock)
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

    (defvar az-org-clock-factor-property "CLOCK_FACTOR"
      "Property with a factor for clocked time, for example 1.25 for after hours.")

    (defun az--org-clock-factor-value (value headline)
      "Return the clock factor VALUE as a number, or 1 if VALUE is nil.
HEADLINE names the heading in the error message."
      (cond ((null value) 1)
            ((string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value)
             (string-to-number value))
            (t (user-error "Invalid %s %S in %S"
                           az-org-clock-factor-property value headline))))

    (defun az--org-clock-factor (entry)
      "Return the clock factor of clocktable ENTRY, or 1 if it has none."
      (az--org-clock-factor-value
       (cdr (assoc az-org-clock-factor-property (nth 5 entry)))
       (nth 1 entry)))

    (defun az--org-clocktable-group-by-cost-center (tables)
      "Merge clocktable TABLES into one table grouped by cost center.
TABLES is a list of tables as `org-clock-get-table-data' returns them.
Each cost center becomes a level 1 row with its sum, followed by its rows.
The level 1 row stores its factored minutes in the property \"Factored\"."
      (let ((groups nil)
            (total 0))
        (dolist (table tables)
          (setq total (+ total (or (nth 1 table) 0)))
          ;; Each element is (LEVEL GROUP ROOT-LEVEL ROW SHARE) for an open ancestor.
          ;; SHARE is (OWN-MINUTES . FACTOR); OWN-MINUTES excludes the child rows.
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
                       (share (cons (nth 4 entry) (az--org-clock-factor entry)))
                       (group-cell (assoc group groups)))
                  (when parent
                    (let ((parent-share (nth 4 parent)))
                      (setcar parent-share (- (car parent-share) (nth 4 entry)))))
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
                    (setq group-cell (list group nil nil nil))
                    (push group-cell groups))
                  ;; group-cell is (GROUP ROWS ROOTS SHARES), all lists in reverse order.
                  (push row (nth 1 group-cell))
                  (when root-p
                    (push row (nth 2 group-cell)))
                  (push share (nth 3 group-cell))
                  (push (list level group root-level row share) ancestors))))))
        (let ((sorted (sort groups
                            (lambda (left right)
                              (cond ((equal (car left) az-org-no-cost-center) nil)
                                    ((equal (car right) az-org-no-cost-center) t)
                                    (t (string< (car left) (car right)))))))
              (entries nil))
          (dolist (group-cell sorted)
            (let ((sum 0)
                  (factored 0))
              (dolist (root (nth 2 group-cell))
                (setq sum (+ sum (nth 4 root))))
              (dolist (share (nth 3 group-cell))
                (setq factored (+ factored (* (car share) (cdr share)))))
              ;; A parent can lose all its time to child rows of other groups.
              (unless (= sum 0)
                (push (list 1 (car group-cell) nil nil sum
                            (list (cons "Factored" factored)))
                      entries)
                (dolist (row (reverse (nth 1 group-cell)))
                  (unless (= (nth 4 row) 0)
                    (push row entries))))))
          (list nil total (nreverse entries)))))

    (defun az-org-clocktable-by-cost-center (ipos tables params)
      "Clocktable formatter that groups rows by `az-org-cost-center-property'.
The clocktable needs :inherit-props t and
:properties (\"COST_CENTER\" \"CLOCK_FACTOR\").
The Factored column shows the time multiplied by `az-org-clock-factor-property'.
With :az-total-only t the table shows only the total row."
      ;; Org collects only the listed properties, so a missing one gives wrong sums.
      (dolist (property (list az-org-cost-center-property az-org-clock-factor-property))
        (unless (member property (plist-get params :properties))
          (user-error "Clocktable needs %S in :properties" property)))
      (let* ((table (az--org-clocktable-group-by-cost-center tables))
             (factored-total 0))
        ;; The default writer prints property values, so format the minutes here.
        (dolist (row (nth 2 table))
          (let ((factored (assoc "Factored" (nth 5 row))))
            (when (and factored (= (nth 0 row) 1))
              (setq factored-total (+ factored-total (cdr factored)))
              (setcdr factored (org-duration-from-minutes (cdr factored))))))
        ;; The rows are still needed above to compute the factored total.
        (when (plist-get params :az-total-only)
          (setf (nth 2 table) nil))
        (prog1
            (org-clocktable-write-default
             ipos
             (list table)
             ;; The group row adds one level, so allow one more time column.
             (org-combine-plists params
                                 (list :properties '("Factored")
                                       :multifile nil
                                       :maxlevel (1+ (or (plist-get params :maxlevel) 3)))))
          ;; The default writer leaves the Factored cell of the total row empty.
          ;; Table line 2 is the total row, because org-table-put skips hlines.
          (save-excursion
            (goto-char ipos)
            (skip-chars-forward "^|")
            (org-table-put 2 1 (format "*%s*" (org-duration-from-minutes factored-total)) t)
            ;; The writer puts property columns first; move Factored after Headline.
            (org-table-goto-column 1)
            (org-table-move-column-right)))))

    (defun az--org-hundredths-string (hundredths)
      "Format HUNDREDTHS of an hour as hours with two decimals."
      (format "%d.%02dh" (/ hundredths 100) (% hundredths 100)))

    (defun az--org-clock-factor-line (start end factor day-start day-end)
      "Describe the part of the clock from START to END inside one day.
The day runs from DAY-START to DAY-END.
FACTOR is the property value as a string.
Return (TEXT . FACTORED-HUNDREDTHS), or nil if the clock is outside the day."
      (let ((clipped-start (if (time-less-p start day-start) day-start start))
            (clipped-end (if (time-less-p day-end end) day-end end)))
        (when (time-less-p clipped-start clipped-end)
          (let* ((minutes (round (float-time (time-subtract clipped-end clipped-start)) 60))
                 ;; Round half up, so a reviewer gets the same result by hand.
                 (hundredths (floor (+ (/ (* minutes 100) 60.0) 0.5)))
                 (factored (floor (+ (* hundredths (string-to-number factor)) 0.5))))
            (cons (format "%s-%s = %s * %s = %s"
                          (format-time-string "%H:%M" clipped-start)
                          (if (time-equal-p clipped-end day-end)
                              "24:00"
                            (format-time-string "%H:%M" clipped-end))
                          (az--org-hundredths-string hundredths)
                          factor
                          (az--org-hundredths-string factored))
                  factored)))))

    (defun az--org-clock-factor-report (day)
      "Return the clocks on DAY whose factor is not 1, as text.
DAY is any time on that day.  Clocks are clipped to the day and sorted by start."
      (let* ((decoded (decode-time day))
             (day-start (org-encode-time 0 0 0
                                         (decoded-time-day decoded)
                                         (decoded-time-month decoded)
                                         (decoded-time-year decoded)))
             (day-end (org-encode-time 0 0 0
                                       (1+ (decoded-time-day decoded))
                                       (decoded-time-month decoded)
                                       (decoded-time-year decoded)))
             (found nil))
        (dolist (file (org-agenda-files))
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (goto-char (point-min))
             (while (re-search-forward org-clock-line-re nil t)
               (let ((clock (org-element-at-point)))
                 (when (and (eq (org-element-type clock) 'clock)
                            (eq (org-element-property :status clock) 'closed)
                            (not (org-before-first-heading-p)))
                   (let* ((headline (save-excursion
                                      (org-back-to-heading t)
                                      (org-get-heading t t t t)))
                          (factor (org-entry-get nil az-org-clock-factor-property t))
                          (timestamp (org-element-property :value clock))
                          (start (org-timestamp-to-time timestamp))
                          (line (and factor
                                     (/= (az--org-clock-factor-value factor headline) 1)
                                     (az--org-clock-factor-line
                                      start
                                      (org-timestamp-to-time timestamp t)
                                      factor day-start day-end))))
                     (when line
                       (push (list start headline line) found)))))))))
        (let ((lines (list (format-time-string "%Y-%m-%d" day-start)))
              (last-headline nil)
              (total 0))
          (dolist (clock (sort found (lambda (left right)
                                       (time-less-p (car left) (car right)))))
            (let ((headline (nth 1 clock))
                  (line (nth 2 clock)))
              (unless (equal headline last-headline)
                (push headline lines)
                (setq last-headline headline))
              (push (car line) lines)
              (setq total (+ total (cdr line)))))
          (push (format "Total = %s" (az--org-hundredths-string total)) lines)
          (string-join (nreverse lines) "\n"))))

    (defun az-org-clock-factor-report (day)
      "Show the clocks on DAY whose factor is not 1, and copy them.
Each line shows the clock times, the hours, the factor and the result."
      (interactive (list (org-read-date nil t)))
      (let ((report (az--org-clock-factor-report day))
            (buffer (get-buffer-create "*Clock factor report*")))
        (kill-new report)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert report))
          (special-mode))
        (display-buffer buffer)
        (message "Clock factor report copied to the kill ring")))

    ;; Clocking keybindings
    (keymap-global-set "C-x C-d" #'org-clock-mark-default-task)))

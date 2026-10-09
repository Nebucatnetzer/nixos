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

    (defun az/org-cc-update-clocktable ()
      "Update clocktable when C-c C-c is pressed anywhere inside one."
      (when (org-in-clocktable-p)
        (org-clock-report)
        t))

    (add-hook 'org-ctrl-c-ctrl-c-hook #'az/org-cc-update-clocktable)

    ;; Clocking keybindings
    (keymap-global-set "C-x C-d" #'org-clock-mark-default-task)))

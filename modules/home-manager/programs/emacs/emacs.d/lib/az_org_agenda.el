;; -*- lexical-binding: t; -*-
(when (boundp 'enable-org)
  (use-package org
    :config
    ;; Agenda faces
    (set-face-attribute 'org-agenda-structure nil :inherit 'default :height 1.00)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.00 :weight 'medium)
    (set-face-attribute 'org-agenda-calendar-event nil :weight 'medium)
    (set-face-attribute 'org-agenda-date nil :inherit 'default :height 1.00 :weight 'bold)
    (set-face-attribute 'org-agenda-date-today nil :slant 'normal :weight 'bold :height 1.00)

    (setopt org-agenda-block-separator " "

            org-todo-keyword-faces
            `(("WAITING"   :foreground "#0087ff" :weight bold)
              ("TODO" :foreground "#d75f00" :weight bold)
              ("PROJECT"      :foreground "#626262" :weight bold)
              ("NEXT"      :foreground "#d70000" :weight bold)))

    (defun az/custom-agenda (&optional arg)
      (interactive "P")
      (org-agenda arg "A"))

    ;; hide done tasks in the agenda
    (setopt org-agenda-skip-deadline-if-done t
            org-agenda-skip-scheduled-if-done t
            org-agenda-skip-timestamp-if-done t

            ;; Custom agenda command to list the stuck projects in the normal
            ;; agenda view.
            org-stuck-projects '("/PROJECT" ("NEXT") nil ""))

    (setq org-agenda-custom-commands (quote (("A" "Custom Agenda"
                                              ((agenda "" nil)
                                               (stuck ""
                                                      ((org-agenda-overriding-header "Stuck Projects")
                                                       (org-agenda-sorting-strategy
                                                        '(priority-down category-up))))
                                               (tags-todo "TODO=\"PROJECT\" "
                                                          ((org-agenda-overriding-header "Projects")
                                                           (org-agenda-sorting-strategy
                                                            '(priority-down category-up))))
                                               nil))
                                             ;; Show all headings with the corresponding TODO state
                                             ("N" occur-tree "NEXT")
                                             ("O" occur-tree "TODO")
                                             ("W" occur-tree "WAITING"))))

    (defun my/org-agenda-skip-if-project-tagged-or-dated ()
      "Skip entry if it is a PROJECT, has a 'skip-agenda' tag, or has any date.
This function is intended for use with `org-agenda-skip-function'."
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (todo-state (org-get-todo-state))
            (tags (org-get-tags-at)))
        (if (or
             ;; Condition 1: The task has any kind of date (scheduled, deadline, or timestamp).
             (org-get-scheduled-time (point))
             (org-get-deadline-time (point))
             (org-entry-get (point) "TIMESTAMP")

             ;; Condition 2: The task's TODO keyword is "PROJECT".
             (and todo-state (string= todo-state "PROJECT"))

             ;; Condition 3: The task is tagged with "skip-agenda".
             (member "skipagenda" tags))
            ;; If any condition is met, return the position of the next headline to skip this entry.
            next-headline
          ;; Otherwise, return nil to include the entry in the agenda.
          nil)))

    (add-to-list 'org-agenda-custom-commands
                 '("n" "Tasks to Plan (sorted, by category)"
                   ((alltodo ""
                             ((org-agenda-skip-function
                               '(my/org-agenda-skip-if-project-tagged-or-dated))
                              (org-agenda-sorting-strategy
                               '(category-keep priority-down))))))
                 'append)

    (setq org-agenda-sorting-strategy (quote
                                       ((agenda priority-down todo-state-up category-up))))

    ;; don't show the warnings for deadlines if the item is scheduled
    (setopt org-agenda-skip-deadline-prewarning-if-scheduled t

            org-agenda-prefix-format '((agenda . " %i %-25:c%?-12t% s")
                                       (todo . " %i %-25:c")
                                       (tags . " %i %-25:c")
                                       (search . " %i %-25:c"))

            ;; start the agenda on the current day and show the next 13 days
            org-agenda-span 8
            org-agenda-start-on-weekday nil
            org-agenda-show-future-repeats (quote next)

            ;; dimm open tasks
            org-agenda-dim-blocked-tasks t

            ;; Put the tags in a more visible spot
            org-agenda-tags-column -120)

    ;; automatically refresh the agenda after adding a task
    (add-hook 'org-capture-after-finalize-hook 'az-org-agenda-redo)

    (defun az-org-agenda-redo ()
      (interactive)
      (when (get-buffer "*Org Agenda*")
        (with-current-buffer "*Org Agenda*"
          (org-agenda-redo t)
          (message "[org agenda] refreshed!"))))

    (load-library "find-lisp")
    (defun az-update-org-agenda-files ()
      "Update the list of org-agenda-files dynamically."
      (setq org-agenda-files
            (append (find-lisp-find-files az-org-inbox-dir "\\.org$")
                    (find-lisp-find-files az-org-projects-dir "\\.org$"))))

    ;; Add a hook to update agenda files whenever org-agenda is invoked
    (add-hook 'org-agenda-mode-hook #'az-update-org-agenda-files)

    (when (boundp 'enable-clocking)
      (require 'org-clock)

      (defvar az-org-daily-work-minutes 510
        "Target number of minutes to work per day (8.5 hours).")

      (defvar az-org-weekly-work-minutes 2040
        "Target number of minutes to work per week (34 hours).")

      (defun az/org-clocked-minutes-in-range (range)
        "Total minutes clocked within RANGE across `org-agenda-files'.
RANGE is a (START END) pair as returned by `org-clock-special-range'.
Includes the still-running clock, if any."
        (let ((total 0))
          (dolist (file (org-agenda-files))
            (with-current-buffer (find-file-noselect file)
              (setq total (+ total (or (org-clock-sum (car range) (cadr range)) 0)))))
          (when (org-clocking-p)
            (setq total (+ total (floor (float-time (time-since org-clock-start-time)) 60))))
          total))

      (defun az/org-clocked-minutes-today ()
        "Total minutes clocked today across `org-agenda-files'."
        (az/org-clocked-minutes-in-range (org-clock-special-range 'today)))

      (defun az/org-clocked-minutes-this-week ()
        "Total minutes clocked this week across `org-agenda-files'."
        (az/org-clocked-minutes-in-range (org-clock-special-range 'thisweek)))

      (defun az/org-format-minutes (minutes)
        "Format MINUTES as \"Xh YYm\"."
        (format "%dh %02dm" (/ minutes 60) (% minutes 60)))

      (defun az/org-agenda-work-until-string ()
        "Describe when today's clocked time reaches `az-org-daily-work-minutes'.
Turns green (face `success') once the target is reached."
        (let* ((worked (az/org-clocked-minutes-today))
               (remaining (- az-org-daily-work-minutes worked)))
          (if (<= remaining 0)
              (propertize (format "Worked %s today - 8.5h reached %s ago"
                                  (az/org-format-minutes worked)
                                  (az/org-format-minutes (- remaining)))
                          'face 'success)
            (format "Worked %s today - work until %s to reach 8.5h"
                    (az/org-format-minutes worked)
                    (format-time-string "%H:%M" (time-add (current-time) (* remaining 60)))))))

      (defun az/org-agenda-week-summary-string ()
        "Describe when this week's clocked time reaches `az-org-weekly-work-minutes'.
Turns green (face `success') once the target is reached."
        (let* ((worked (az/org-clocked-minutes-this-week))
               (target (az/org-format-minutes az-org-weekly-work-minutes))
               (remaining (- az-org-weekly-work-minutes worked)))
          (if (<= remaining 0)
              (propertize (format "Worked %s this week - %s reached (%s over)"
                                  (az/org-format-minutes worked) target
                                  (az/org-format-minutes (- remaining)))
                          'face 'success)
            ;; Unlike the daily projection, this can roll past midnight, so
            ;; the weekday is included to keep the timestamp unambiguous.
            (format "Worked %s this week - work until %s to reach %s"
                    (az/org-format-minutes worked)
                    (format-time-string "%a %H:%M" (time-add (current-time) (* remaining 60)))
                    target))))

      (defun az/org-agenda-insert-summary-lines ()
        "Insert the weekly and daily work summaries at the top of the agenda.
`org-agenda-finalize-hook' also runs after single-line updates (e.g.
toggling a TODO state), with the buffer narrowed to just that one
line - skip in that case, or the lines get inserted mid-agenda
instead of at the real top."
        (when (and (derived-mode-p 'org-agenda-mode) (not (buffer-narrowed-p)))
          (goto-char (point-min))
          (insert (az/org-agenda-week-summary-string) "\n"
                  (az/org-agenda-work-until-string) "\n\n")))

      (add-hook 'org-agenda-finalize-hook #'az/org-agenda-insert-summary-lines))))

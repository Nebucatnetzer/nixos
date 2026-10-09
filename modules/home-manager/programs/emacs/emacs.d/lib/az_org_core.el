;; -*- lexical-binding: t; -*-
(when (bound-and-true-p enable-org)
  (use-package ox-pandoc
    :after org)

  (use-package org
    :bind (("<f9>" . az/custom-agenda)
           :map az-map
           ("C-a" . org-agenda)
           ("C-c" . org-capture)
           ("C-s" . org-store-link)
           :map org-mode-map
           ("C-c C-," . org-insert-structure-template)
           ("C-c C-$" . org-archive-subtree))
    :config
    (require 'org-indent)

    (setopt org-startup-indented t
            org-indent-mode-turns-on-hiding-stars nil
            )

    (defun az/apply-font-settings (frame)
      "Apply font settings when a new FRAME is created."
      (with-selected-frame frame
        (set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
        (dolist (face '((org-level-1 . 1.35)
                        (org-level-2 . 1.3)
                        (org-level-3 . 1.2)
                        (org-level-4 . 1.1)
                        (org-level-5 . 1.1)
                        (org-level-6 . 1.1)
                        (org-level-7 . 1.1)
                        (org-level-8 . 1.1)))
          (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

        ;; Make the document title a bit bigger
        (set-face-attribute 'org-document-title nil :weight 'bold :height 1.7)

        (set-face-attribute 'org-document-info nil         :inherit '(shadow fixed-pitch) :height 0.8 :slant 'italic :foreground 'unspecified)
        (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch) :height 0.8 :slant 'italic :foreground 'unspecified)

        (set-face-attribute 'org-block nil :foreground 'unspecified  :inherit 'fixed-pitch)
        (set-face-attribute 'org-checkbox nil              :inherit 'fixed-pitch)
        (set-face-attribute 'org-code nil                  :inherit 'fixed-pitch)
        (set-face-attribute 'org-date nil                  :inherit '(shadow fixed-pitch) :height 0.8)
        (set-face-attribute 'org-drawer nil                :inherit 'fixed-pitch :height 0.8)
        (set-face-attribute 'org-indent nil                :inherit '(org-hide fixed-pitch))
        (set-face-attribute 'org-meta-line nil             :inherit 'fixed-pitch :height 0.8)
        (set-face-attribute 'org-special-keyword nil       :inherit 'fixed-pitch :height 0.8)
        (set-face-attribute 'org-table nil                 :inherit 'fixed-pitch)
        (set-face-attribute 'org-verbatim nil              :inherit '(shadow fixed-pitch))
        (plist-put org-format-latex-options :scale 2)))

    ;; Apply when a new frame is created
    (add-hook 'after-make-frame-functions #'az/apply-font-settings)

    ;; Also apply immediately if not in daemon mode, or if a frame already exists
    (when (display-graphic-p)
      (az/apply-font-settings (selected-frame)))

    (setopt org-tags-column 0
            org-use-tag-inheritance t

            ;; disable line split with M-RET
            org-M-RET-may-split-line (quote ((default)))

            ;; Allow headings with visibility folded to get folded when opening a file
            org-startup-folded 'nofold

            ;; enable the correct intdentation for source code blocks
            org-edit-src-content-indentation 0
            org-src-tab-acts-natively t
            org-src-preserve-indentation t

            ;; enable todo and checkbox depencies
            org-enforce-todo-dependencies t
            org-enforce-todo-checkbox-dependencies t

            ;; quick access for todo states
            org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "WAITING(w!)" "PROJECT(p)" "|" "DONE(d)")
              (sequence "|" "CANCELLED(c)"))

            org-log-done 'time
            org-log-into-drawer t)

    ;; capture templates
    (defun az-org-capture-read-file-name ()
      (concat (expand-file-name (read-file-name "PROMPT: " az-org-inbox-dir)) ".org"))

    (setopt org-capture-templates
            `(("t" "Adds a Next entry" entry
               (file+headline ,az-org-inbox-file "Capture")
               (file ,(concat az-org-templates-dir "temp_personal_todo.txt"))
               :clock-in t
               :clock-resume t
               :empty-lines 1)
              ("n" "Add note" plain (file az-org-capture-read-file-name)
               (file ,(concat az-org-templates-dir "temp_note.txt")))
              )

            ;; org-refile options
            org-refile-allow-creating-parent-nodes (quote confirm)
            org-refile-use-outline-path 'file
            org-outline-path-complete-in-steps nil)

    ;; Runs while the capture buffer is still narrowed to the new entry, so
    ;; `org-entry-put' cannot walk back past it into the wrong heading.
    (defun az/org-capture-stamp-created ()
      "Stamp the entry being captured with the time of capture."
      (when (and (not org-note-abort)
                 (eq (org-capture-get :type 'local) 'entry))
        (save-excursion
          (org-entry-put (point) "CREATED"
                         (format-time-string "[%Y-%m-%d %a %H:%M]")))))

    (add-hook 'org-capture-prepare-finalize-hook #'az/org-capture-stamp-created)
    (defun az/org-fold-finished-entries ()
      "Fold the body of every DONE and CANCELLED entry in a file buffer to make files look a bit tidier.
Scope nil, not 'file: 'file prompts for files not yet on disk, such as
a new archive file."
      (when buffer-file-name
        (org-map-entries #'org-fold-hide-subtree
                         "/DONE|CANCELLED" nil 'archive 'comment)))

    (add-hook 'org-mode-hook #'az/org-fold-finished-entries)

    (defun az-org-files-list ()
      (delq nil
            (mapcar (lambda (buffer)
                      (buffer-file-name buffer))
                    (org-buffer-list 'files t))))

    (setopt org-refile-targets '((az-org-files-list :maxlevel . 6))

            org-src-fontify-natively t

            org-highlight-latex-and-related '(latex)

            org-image-actual-width (quote (500))
            org-startup-with-inline-images t

            org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
            org-clone-delete-id t

            org-blank-before-new-entry
            (quote ((heading . t)
                    (plain-list-item . auto))))

    ;; org faces
    ;; alabaster-themes-light-bg has no org faces, so take the colors from its palette.
    (alabaster-themes-with-colors
      (set-face-attribute 'org-done nil :foreground green :weight 'bold)
      (set-face-attribute 'org-link nil :foreground link :underline t)
      (set-face-attribute 'org-scheduled nil :foreground green :slant 'italic :weight 'normal)
      (set-face-attribute 'org-scheduled-previously nil :foreground red :weight 'normal)
      (set-face-attribute 'org-scheduled-today nil :foreground green :slant 'italic :weight 'normal)
      (set-face-attribute 'org-todo nil :background 'unspecified :foreground red :weight 'bold)
      (set-face-attribute 'org-upcoming-deadline nil :foreground red :weight 'normal)
      (set-face-attribute 'org-warning nil :foreground red :weight 'normal)
      ;; Remove org's own foreground so the inherited shadow face shows.
      (set-face-attribute 'org-date nil :foreground 'unspecified)
      (set-face-attribute 'org-drawer nil :foreground 'unspecified))

    (defun az-org-archive-location ()
      "Return the archive location for the current month."
      (concat az-org-archive-dir
              (format-time-string "%Y") "/projects/"
              (format-time-string "%Y-%m") "-%s::datetree/"))

    ;; The daemon runs for days. Refresh the month before each archive.
    (defun az-org-refresh-archive-location (&rest _)
      "Set `org-archive-location' for the current month."
      (setq org-archive-location (az-org-archive-location)))

    (advice-add 'org-archive-subtree :before #'az-org-refresh-archive-location)

    (setopt org-attach-id-dir "resources/"
            org-archive-location (az-org-archive-location))

    (defun org-update-cookies-after-save()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (org-update-statistics-cookies "ALL")))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)))

    (add-hook 'org-mode-hook #'az-prose-editing)
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

    ;; Calender should start on Monday
    (setopt calendar-week-start-day 1)

    ;; org-checklist resets checkboxes when a repeating task is marked done.
    (require 'org-checklist)

    ;; --- Keybindings ---

    ;; Calendar date entry navigation
    (dolist (binding '(("M-h" . calendar-backward-day)
                       ("M-l" . calendar-forward-day)
                       ("M-k" . calendar-backward-week)
                       ("M-j" . calendar-forward-week)
                       ("M-H" . calendar-backward-month)
                       ("M-L" . calendar-forward-month)
                       ("M-K" . calendar-backward-year)
                       ("M-J" . calendar-forward-year)))
      (let ((calendar-command (cdr binding)))
        (keymap-set org-read-date-minibuffer-local-map (car binding)
                    (lambda ()
                      (interactive)
                      (org-eval-in-calendar (list calendar-command 1)))))))

  ;; Load additional org config files.
  (load-file (modules-path "az_org_babel.el"))
  (load-file (modules-path "az_org_clocking.el"))
  (load-file (modules-path "az_org_agenda.el"))
  (load-file (modules-path "az_org_export.el"))
  (load-file (modules-path "az_org_insert.el"))
  (load-file (modules-path "az_org_log.el"))
  (load-file (modules-path "az_org_gitlab.el")))

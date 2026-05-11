;; -*- lexical-binding: t; -*-
(when (boundp 'enable-org)
  (use-package ox-pandoc
    :after org)

  (use-package org
    :bind (("C-c a" . org-agenda)
           ("C-c l" . org-store-link)
           ("C-c c" . org-capture)
           ("<f9>" . az/custom-agenda)
           :map org-mode-map
           ("C-c C-," . org-insert-structure-template)
           ("C-c C-$" . org-archive-subtree))
    :hook
    (org-mode-hook . (lambda () (org-map-entries #'org-fold-hide-subtree
                                                 "/+DONE" 'file 'archive 'comment)
                       (org-map-entries #'org-fold-hide-subtree
                                        "/+CANCELLED" 'file 'archive 'comment)))
    :config
    (require 'org-indent)

    ;; When you don't have a frame initialised (terminal mode) emacs doesn't
    ;; doesn't want to open links in the browser because it thinks you're not
    ;; running a GUI. This fixes this.
    ;; Force Org mode to use your browser function and rebuild the full URL
    (org-link-set-parameters "http"
                             :follow (lambda (path &rest _args)
                                       (funcall browse-url-browser-function (concat "http:" path))))
    (org-link-set-parameters "https"
                             :follow (lambda (path &rest _args)
                                       (funcall browse-url-browser-function (concat "https:" path))))

    ;; Intercept http and https links and force them to your graphical browser
    (add-to-list 'browse-url-handlers '("\\`http" . browse-url-generic))

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

        (set-face-attribute 'org-block nil :foreground 'unspecified  :inherit 'fixed-pitch)
        (set-face-attribute 'org-checkbox nil              :inherit 'fixed-pitch)
        (set-face-attribute 'org-code nil                  :inherit 'fixed-pitch)
        (set-face-attribute 'org-date nil                  :inherit '(shadow fixed-pitch) :height 0.8)
        (set-face-attribute 'org-document-info nil         :inherit 'fixed-pitch :height 0.8 :slant 'italic :foreground "#93a1a1")
        (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch :height 0.8 :slant 'italic :foreground "#93a1a1")
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
      (concat (expand-file-name (read-file-name "PROMPT: " "~/nextcloud/01_inbox/")) ".org"))

    (setopt org-capture-templates
            (quote
             (("t" "Adds a Next entry" entry
               (file+headline "~/nextcloud/01_inbox/00_inbox.org" "Capture")
               (file "~/nextcloud/99_archive/0000/settings/templates/temp_personal_todo.txt")
               :clock-in t
               :clock-resume t
               :empty-lines 1)
              ("n" "Add note" plain (file az-org-capture-read-file-name)
               (file "~/nextcloud/99_archive/0000/settings/templates/temp_note.txt"))
              ))

            ;; org-refile options
            org-refile-allow-creating-parent-nodes (quote confirm)
            org-refile-use-outline-path 'file
            org-outline-path-complete-in-steps nil)

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
    (set-face-attribute 'org-done nil :foreground "#5f8700" :weight 'bold)
    (set-face-attribute 'org-link nil :foreground "#0087ff" :underline t)
    (set-face-attribute 'org-scheduled nil :foreground "#5f8700" :slant 'italic :weight 'normal)
    (set-face-attribute 'org-scheduled-previously nil :foreground "#d70000" :weight 'normal)
    (set-face-attribute 'org-scheduled-today nil :foreground "#5f8700" :slant 'italic :weight 'normal)
    (set-face-attribute 'org-todo nil :background "nil" :foreground "#d70000" :weight 'bold)
    (set-face-attribute 'org-upcoming-deadline nil :foreground "#d70000" :weight 'normal)
    (set-face-attribute 'org-warning nil :foreground "#d70000" :weight 'normal)

    (setopt org-attach-id-dir "resources/"

            org-archive-location
            (concat "~/nextcloud/99_archive/"
                    (format-time-string "%Y" (current-time)) "/projects/"
                    (format-time-string "%Y-%m" (current-time)) "-%s::datetree/"))

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

    (add-hook 'org-mode-hook
              (defun az-org-editing ()
                "My settings for message composition."
                (setq whitespace-line-column 500)
                (olivetti-mode)
                (turn-off-auto-fill)))
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

    ;; Calender should start on Monday
    (setopt calendar-week-start-day 1

            ;; Enable additional org modules
            ;; org-checklist to un-toggle checklists when a repeating task gets set to done
            org-modules
            '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe
                      ol-rmail ol-w3m org-checklist))

    ;; --- Keybindings ---

    ;; verb-command-map must be bound as a keymap variable, not via :bind
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
    ;; disable hyperbole's C-c / so org-sparse-tree takes it
    (define-key hyperbole-mode-map (kbd "C-c /") nil)

    ;; Calendar date entry navigation
    (define-key org-read-date-minibuffer-local-map (kbd "M-h")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-l")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-k")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-j")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-H")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-L")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-K")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-J")
                (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

    (when (boundp 'enable-clocking)
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
        (start-heading-clock "f4294c36-0b69-4a9e-a5d9-54c924011bf0" "~/nextcloud/02_projects/00_work.org"))

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
      (global-set-key (kbd "<f6>") #'start-main-clock)
      (global-set-key (kbd "<f7>") #'org-clock-in)
      (global-set-key (kbd "<f8>") #'org-clock-out)
      (global-set-key (kbd "C-x C-d") #'org-clock-mark-default-task)
      )))

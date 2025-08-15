;; -*- lexical-binding: t; -*-
(when (boundp 'enable-org)
  (use-package ox-pandoc
    :after org)

  (use-package org
    :bind (:map org-mode-map
                ("C-c C-," . org-insert-structure-template)
                ("C-c C-$" . org-archive-subtree))
    :hook
    (org-mode-hook . (lambda () (org-map-entries #'org-fold-hide-subtree
                                                 "/+DONE" 'file 'archive 'comment)
                       (org-map-entries #'org-fold-hide-subtree
                                        "/+CANCELLED" 'file 'archive 'comment)))
    :config
    (require 'org-indent)

    ;; enable org-mode keys
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-iswitchb)
    (define-key hyperbole-mode-map (kbd "C-c /") nil)

    ;; comes from the package verb ./az_verb.el
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)

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

        (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
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

    ;; Resize Org headings
    (setopt org-tags-column 0)

    ;; disable line split with M-RET
    (setq org-M-RET-may-split-line (quote ((default))))

    ;; Allow headings with visibility folded to get folded when opening a file
    (setq org-startup-folded 'nofold)

    ;; enable the correct intdentation for source code blocks
    (setq org-edit-src-content-indentation 0)
    (setq org-src-tab-acts-natively t)
    (setq org-src-preserve-indentation t)

    ;; enable todo and checkbox depencies
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)

    ;; quick access for todo states
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "WAITING(w!)" "PROJECT(p)" "|" "DONE(d)")
            (sequence "|" "CANCELLED(c)")))

    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    ;; capture templates
    (defun az-org-capture-read-file-name ()
      (concat (expand-file-name (read-file-name "PROMPT: " "~/nextcloud/01_inbox/")) ".org"))

    (setq org-capture-templates
          (quote
           (("t" "Adds a Next entry" entry
             (file+headline "~/nextcloud/01_inbox/00_inbox.org" "Capture")
             (file "~/nextcloud/99_archive/0000/settings/templates/temp_personal_todo.txt")
             :clock-in t
             :clock-resume t
             :empty-lines 1)
            ("n" "Add note" plain (file az-org-capture-read-file-name)
             (file "~/nextcloud/99_archive/0000/settings/templates/temp_note.txt"))
            )))

    ;; org-refile options
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)

    (defun az-org-files-list ()
      (delq nil
            (mapcar (lambda (buffer)
                      (buffer-file-name buffer))
                    (org-buffer-list 'files t))))

    (setq org-refile-targets '((az-org-files-list :maxlevel . 6)))

    (setq org-src-fontify-natively t)

    (setq org-highlight-latex-and-related '(latex))

    (setq org-image-actual-width (quote (500)))
    (setq org-startup-with-inline-images t)

    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
          org-clone-delete-id t)

    (setq org-blank-before-new-entry
          (quote ((heading . t)
                  (plain-list-item . auto))))

    (set-face-attribute 'org-agenda-structure nil :inherit 'default :height 1.00)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.00 :weight 'medium)
    (set-face-attribute 'org-agenda-calendar-event nil :weight 'medium)
    (set-face-attribute 'org-agenda-date nil :inherit 'default :height 1.00 :weight 'bold)
    (set-face-attribute 'org-agenda-date-today nil :slant 'normal :weight 'bold :height 1.00)
    (set-face-attribute 'org-done nil :foreground "#5f8700" :weight 'bold)
    (set-face-attribute 'org-link nil :foreground "#0087ff" :underline t)
    (set-face-attribute 'org-scheduled nil :foreground "#5f8700" :slant 'italic :weight 'normal)
    (set-face-attribute 'org-scheduled-previously nil :foreground "#d70000" :weight 'normal)
    (set-face-attribute 'org-scheduled-today nil :foreground "#5f8700" :slant 'italic :weight 'normal)
    (set-face-attribute 'org-todo nil :background "nil" :foreground "#d70000" :weight 'bold)
    (set-face-attribute 'org-upcoming-deadline nil :foreground "#d70000" :weight 'normal)
    (set-face-attribute 'org-warning nil :foreground "#d70000" :weight 'normal)

    (setq org-startup-shrink-all-tables t)

    ;; org-export formats
    (setq org-export-backends (quote (beamer html latex md odt reveal)))

    (setq org-html-html5-fancy t
          org-html-doctype "html5")

    ;; disable the Todo keywords in the export
    (setq org-export-with-todo-keywords nil)

    ;; disable the tags in the export
    (setq org-export-with-tags nil)

    ;; place captions below images
    (setq org-latex-caption-above nil)

    (setq org-export-with-sub-superscripts nil) ;; why is this needed?

    (setq org-export-with-smart-quotes t)

    (setq org-export-headline-levels 5)

    ;; options for beamer exports
    (setq org-beamer-frame-level 2)
    (setq org-beamer-outline-frame-options "")
    (setq org-beamer-outline-frame-title "Inhalt")
    (setq org-beamer-theme "metropolis")


    ;; enable org-mode keys
    (global-set-key "\C-cl"'org-store-link)
    (global-set-key "\C-cc" 'org-capture)

    ;; evilificate calendar in org-mode
    (define-key org-read-date-minibuffer-local-map (kbd "M-h")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-l")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-k")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-j")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-H")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-L")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-K")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-J")
                (lambda ()
                  (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

    ;; evil keybindings for the org-agenda
    (evil-add-hjkl-bindings org-agenda-mode-map 'emacs
      ;;(kbd "/")       'evil-search-forward
      (kbd "n")       'evil-search-next
      (kbd "N")       'evil-search-previous
      (kbd "C-d")     'evil-scroll-down
      (kbd "C-u")     'evil-scroll-up
      (kbd "c")       'org-capture
      (kbd "$" )       'evil-end-of-line
      (kbd "SPC") 'god-execute-with-current-bindings
      (kbd "C-w C-w") 'other-window)

    (setq org-attach-id-dir "resources/")

    (setq org-todo-keyword-faces
          `(("WAITING"   :foreground "#0087ff" :weight bold)
            ("TODO" :foreground "#d75f00" :weight bold)
            ("PROJECT"      :foreground "#626262" :weight bold)
            ("NEXT"      :foreground "#d70000" :weight bold)))

    ;; options for latex exports
    (setq org-latex-classes
          (quote
           (("beamer" "\\documentclass{beamer}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
            ("article" "\\documentclass{article}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
            ("report" "\\documentclass[11pt]{report}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
            ("book" "\\documentclass[11pt]{book}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
    (setq org-latex-default-packages-alist nil)
    (setq org-latex-listings 'listings)
    (setq org-latex-title-command "\\maketitle\\newpage")
    (setq org-latex-toc-command "\\tableofcontents
  \\newpage
  ")

    ;; Set the agenda separator to a space character.
    (setq org-agenda-block-separator " ")


    ;; Disable tag inheritance
    (setopt org-use-tag-inheritance nil)

    (setq org-archive-location
          (concat "~/nextcloud/99_archive/"
                  (format-time-string "%Y" (current-time)) "/projects/"
                  (format-time-string "%Y-%m" (current-time)) "-%s::datetree/"))

    ;; a function to call the custom agenda view.
    (defun az/custom-agenda (&optional arg)
      (interactive "P")
      (org-agenda arg "A"))

    (global-set-key [f9] 'az/custom-agenda)

    ;; hide done tasks in the agenda
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-timestamp-if-done t)

    ;; Custom agenda command to list the stuck projects in the normal
    ;; agenda view.
    (setq org-stuck-projects '("/PROJECT" ("NEXT") nil ""))
    (setq org-agenda-custom-commands
          (quote (("A" "Custom Agenda"
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

    ;; don't show the warnings for deadlines if the item is scheduled
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)


    (setopt org-agenda-prefix-format
            '((agenda . " %i %-25:c%?-12t% s")
              (todo . " %i %-25:c")
              (tags . " %i %-25:c")
              (search . " %i %-25:c")))

    ;; start the agenda on the current day and show the next 13 days
    (setq org-agenda-span 14
          org-agenda-start-on-weekday nil)
    (setq org-agenda-show-future-repeats (quote next))
    (setq org-agenda-sorting-strategy
          (quote
           ((agenda priority-down todo-state-up category-up))))

    ;; dimm open tasks
    (setq org-agenda-dim-blocked-tasks t)

    ;; Put the tags in a more visible spot
    (setopt org-agenda-tags-column -120)

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
            (append (find-lisp-find-files "~/nextcloud/01_inbox/" "\\.org$")
                    (find-lisp-find-files "~/nextcloud/02_projects/" "\\.org$"))))

    ;; Add a hook to update agenda files whenever org-agenda is invoked
    (add-hook 'org-agenda-mode-hook #'az-update-org-agenda-files)

    (defun org-update-cookies-after-save()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (org-update-statistics-cookies "ALL")))

    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook 'org-mode-hook
              (defun az-org-editing ()
                "My settings for message composition."
                (setq whitespace-line-column 500)
                (olivetti-mode)
                (turn-off-auto-fill)))
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


    ;; https://emacs.stackexchange.com/a/41619
    (defun az-markdown-convert-buffer-to-org ()
      "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
      (interactive)
      (let ((convert-command (format "pandoc -f markdown -t org -o %s"
                                     (concat (shell-quote-argument(file-name-sans-extension (buffer-file-name))) ".org"))))
        (message convert-command)
        (shell-command-on-region (point-min) (point-max)
                                 convert-command)))

    ;; Calender should start on Monday
    (setq calendar-week-start-day 1)

    ;; Enable additional org modules
    ;; org-checklist to un-toggle checklists when a repeating task gets set to done
    (setopt org-modules
            '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe
                      ol-rmail ol-w3m org-checklist))

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

      (global-set-key (kbd "<f6>") 'start-main-clock)

      (org-clock-persistence-insinuate)

      (setq org-clock-out-remove-zero-time-clocks t)
      (setq org-clock-out-when-done t)

      (setq org-clock-persist t)
      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)

      (global-set-key (kbd "<f7>") 'org-clock-in)
      (global-set-key (kbd "<f8>") 'org-clock-out)
      (global-set-key (kbd "C-x C-d") 'org-clock-mark-default-task)

      (setq org-duration-format (quote (("h") (special . 2))))

      (setq org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 4 :tcolumns 3))))

    (setopt org-clocktable-defaults '(:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil
                                                :tend nil :step nil :stepskip0 nil :fileskip0 t :tags nil :match nil
                                                :emphasize nil :link nil :narrow 40! :indent t :filetitle nil
                                                :hidefiles t :formula nil :timestamp nil :level nil :tcolumns nil
                                                :formatter nil))
    ))

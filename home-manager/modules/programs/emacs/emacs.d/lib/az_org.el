;; -*- lexical-binding: t; -*-
(when (boundp 'enable-org)
  ;; Enable pretty bullets in org mode
  (use-package org-superstar
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (org-superstar-mode 1))))

  (use-package ox-pandoc
    :after org)

  (use-package org-modern
    :after org
    :config
    (global-org-modern-mode))

  (use-package org
    :bind (:map org-mode-map
                ;; comes from the package verb ./az_verb.el
                ("C-c C-r" . verb-command-map)
                ("C-c C-," . org-insert-structure-template))
    :config
    ;; enable org-mode keys
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-iswitchb)


    ;; disable line split with M-RET
    (setq org-M-RET-may-split-line (quote ((default))))

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

    ;; enable org-indent
    (setq org-startup-indented t)

    ;; capture templates
    (defun az-org-capture-read-file-name ()
      (concat (expand-file-name (read-file-name "PROMPT: " "~/nextcloud/10_documents/01_inbox/")) ".org"))

    (setq org-capture-templates
          (quote
           (("t" "Adds a Next entry" entry
             (file+headline "~/nextcloud/10_documents/01_inbox/inbox.org" "Capture")
             (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_personal_todo.txt")
             :empty-lines 1)
            ("n" "Add note" plain (file az-org-capture-read-file-name)
             (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_note.txt"))
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

    (setq org-latex-caption-above nil)

    (setq org-export-with-sub-superscripts nil)

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

    (setq org-archive-location
          (concat "~/nextcloud/10_documents/99_archive/"
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
                             '(category-up))))
                    (tags-todo "TODO=\"PROJECT\" "
                               ((org-agenda-overriding-header "Projects")
                                (org-agenda-sorting-strategy
                                 '(category-up))))
                    nil))
                  ;; Show all headings with the corresponding TODO state
                  ("N" occur-tree "NEXT")
                  ("O" occur-tree "TODO")
                  ("W" occur-tree "WAITING"))))

    ;; don't show the warnings for deadlines if the item is scheduled
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

    ;; start the agenda on the current day and show the next 13 days
    (setq org-agenda-span 1
          org-agenda-start-on-weekday nil)
    (setq org-agenda-tags-column -80)
    (setq org-agenda-show-future-repeats (quote next))
    (setq org-agenda-sorting-strategy
          (quote
           ((agenda todo-state-up priority-down category-up))))

    ;; dimm open tasks
    (setq org-agenda-dim-blocked-tasks t)

    ;; automatically refresh the agenda after adding a task
    (add-hook 'org-capture-after-finalize-hook 'az-org-agenda-redo)

    (defun az-org-agenda-redo ()
      (interactive)
      (when (get-buffer "*Org Agenda*")
        (with-current-buffer "*Org Agenda*"
          (org-agenda-redo t)
          (message "[org agenda] refreshed!"))))

    (load-library "find-lisp")
    (setq org-agenda-files
          (append (find-lisp-find-files "~/nextcloud/10_documents/01_inbox/" "\.org$")
                  (find-lisp-find-files "~/nextcloud/10_documents/02_projects/" "\.org$")))

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
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))

  ;; Calender should start on Monday
  (setq calendar-week-start-day 1))

(use-package treemacs
  :ensure t
  :bind ("<f12>" . treemacs)
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)))

(when (boundp 'enable-langtool)
  (use-package langtool
    :ensure t))

;; Amx, an alternative interface for M-x in Emacs
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :ensure t
  :config
  (amx-mode t))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package gnu-elpa-keyring-update
  :ensure t)

;; enable magit a great git porcelain.
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("<f10>" . magit-status))

(use-package bug-hunter
  :defer t
  :ensure t)

(use-package direnv
  :config
  (direnv-mode))

(when (boundp 'enable-org-bullets)
  ;; Enable pretty bullets in org mode
  (use-package org-superstar
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (org-superstar-mode 1)))))

;; add a package to convert the agenda view to HTML
(use-package htmlize
  :ensure t
  :after org)

(when (is-linux-p)
;;; org-man.el - Support for links to manpages in Org

  (org-add-link-type "man" 'org-man-open)
  (add-hook 'org-store-link-functions 'org-man-store-link)

  (defcustom org-man-command 'man
    "The Emacs command to be used to display a man page."
    :group 'org-link
    :type '(choice (const man) (const woman)))

  (defun org-man-open (path)
    "Visit the manpage on PATH.
    PATH should be a topic that can be thrown at the man command."
    (funcall org-man-command path))

  (defun org-man-store-link ()
    "Store a link to a manpage."
    (when (memq major-mode '(Man-mode woman-mode))
      ;; This is a man page, we do make this link
      (let* ((page (org-man-get-page-name))
             (link (concat "man:" page))
             (description (format "Manpage for %s" page)))
        (org-store-link-props
         :type "man"
         :link link
         :description description))))

  (defun org-man-get-page-name ()
    "Extract the page name from the buffer name."
    ;; This works for both `Man-mode' and `woman-mode'.
    (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
        (match-string 1 (buffer-name))
      (error "Cannot create link to this man page")))

  (provide 'org-man)

;;; org-man.el ends here
  (require 'org-man))

(use-package org-ref
  :ensure t
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-default-citation-link "footcite")
  (setq reftex-default-bibliography '("~/nextcloud/03_documents/org/notes/_resources/references.bib"))
  (setq org-ref-bibliography-notes "~/nextcloud/03_documents/org/notes/bibliography_notes.org"
        org-ref-default-bibliography '("~/nextcloud/03_documents/org/notes/_resources/references.bib")
        org-ref-pdf-directory "~/nextcloud/03_documents/org/notes/_resources/"))

(when (boundp 'enable-ox-pandoc)
  (use-package ox-pandoc
    :ensure t
    :after org))

(use-package org
  :ensure t
  :pin gnu
  :config

  ;; enable org-mode keys
  (when (or (boundp 'enable-personal-agenda)
            (boundp 'enable-work-agenda))
    (define-key global-map "\C-ca" 'org-agenda))

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)

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

  ;; disable line split with M-RET
  (setq org-M-RET-may-split-line (quote ((default))))

  ;; enable the correct intdentation for source code blocks
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)

  ;; archive files to a monthly file
  (when (boundp 'enable-personal-agenda)
    (when (is-linux-p)
      (setq org-archive-location
            (concat "~/nextcloud/10_documents/99_archive/2022/projects/"
                    (format-time-string "%Y-%m" (current-time)) "-%s::datetree/"))))
  (when (boundp 'enable-work-agenda)
    (when (is-windows-p)
      (setq org-archive-location
            (concat "~/nextcloud/03_documents/org/archive/work/"
                    (format-time-string "%Y-%m" (current-time)) "-%s::datetree/"))))

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
  (defun my/org-capture-read-file-name ()
    (concat (expand-file-name (read-file-name "PROMPT: " "~/nextcloud/12_tasks/")) ".org"))

  (when (boundp 'enable-personal-agenda)
    (when (is-linux-p)
      (setq org-capture-templates
            (quote
             (("t" "Adds a Next entry" entry
               (file+headline "~/nextcloud/12_tasks/personal.org" "Capture")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_personal_todo.txt")
               :empty-lines 1)
              ("n" "Add note" plain (file my/org-capture-read-file-name)
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_note.txt"))
              )))))

  (when (boundp 'enable-work-agenda)
    (when (is-windows-p)
      (setq org-capture-templates
            (quote
             (("j" "Journal Entry" entry
               (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Clock")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_clock_note.txt")
               :empty-lines 1)
              ("c" "Phone call" entry (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Clock")
               "* %U PHONE %?" :clock-in t :clock-resume t)
              ("t" "Adds a Next entry" entry
               (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Capture")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_work_todo.txt")
               :clock-in t :clock-resume t)
              ("p" "Small Project" entry
               (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Capture")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_work_small_project.txt"))
              ("m" "Meeting" entry (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Capture")
               "* %U MEETING: with %?\n" :clock-in t :clock-resume t :empty-lines 1)
              ("n" "Add note" plain (file my/org-capture-read-file-name)
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_note.txt"))
              )))))

  ;; org-columns format
  (setq org-columns-default-format
        "%40ITEM(Task) %8Effort(Estimated Effort){:} %8CLOCKSUM %10TAGS")

  ;; available effort times
  (setq org-global-properties
        (quote
         (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))))

  ;; org-refile options
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (defun nebucatnetzer-org-files-list ()
    (delq nil
          (mapcar (lambda (buffer)
                    (buffer-file-name buffer))
                  (org-buffer-list 'files t))))

  (setq org-refile-targets '((nebucatnetzer-org-files-list :maxlevel . 6)))

  (setq org-src-fontify-natively t)

  (setq org-highlight-latex-and-related '(latex))

  (setq org-image-actual-width (quote (500)))
  (setq org-startup-with-inline-images t)

  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-clone-delete-id t)

  (setq org-blank-before-new-entry
        (quote ((heading . t)
                (plain-list-item . auto))))

  (setq org-footnote-section "Resources")

  (setq org-attach-directory "~/nextcloud/10_documents/99_archive/2022/resources/")

  (setq org-todo-keyword-faces
        `(("WAITING"   :foreground "#0087ff" :weight bold)
          ("TODO" :foreground "#d75f00" :weight bold)
          ("PROJECT"      :foreground "#626262" :weight bold)
          ("NEXT"      :foreground "#d70000" :weight bold)))


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
  ;;(setq org-export-backends (quote (beamer html latex md odt reveal)))

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
  (setq org-agenda-span 14
        org-agenda-start-on-weekday nil)
  (setq org-agenda-tags-column -80)
  (setq org-agenda-show-future-repeats (quote next))
  (setq org-agenda-sorting-strategy
        (quote
         ((agenda todo-state-up priority-down category-up))))

  ;; dimm open tasks
  (setq org-agenda-dim-blocked-tasks t)

  ;; automatically refresh the agenda after adding a task
  (add-hook 'org-capture-after-finalize-hook 'nebucatnetzer:org-agenda-redo)

  (defun nebucatnetzer:org-agenda-redo ()
    (interactive)
    (when (get-buffer "*Org Agenda*")
      (with-current-buffer "*Org Agenda*"
        (org-agenda-redo t)
        (message "[org agenda] refreshed!"))))

  (setq org-clock-out-remove-zero-time-clocks t)

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
      (start-heading-clock "e9f71012-4370-4dd2-af8e-9ae14d86508a" "~/nextcloud/03_documents/org/agenda/work/work.org"))

    (global-set-key (kbd "<f6>") 'start-main-clock))

  (org-clock-persistence-insinuate)

  (setq org-clock-out-when-done t)

  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  (global-set-key (kbd "<f7>") 'org-clock-in)
  (global-set-key (kbd "<f8>") 'org-clock-out)
  (global-set-key (kbd "C-x C-d") 'org-clock-mark-default-task)

  (setq org-duration-format (quote (("h") (special . 2))))

  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 4 :tcolumns 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; add image from conference phone upload                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; use case is taking a photo of a slide in a conference and uploading
  ;; it to google drive or dropbox or whatever to get it on your
  ;; computer. You then want to embed it in an org-mode document by
  ;; moving it to the same folder and renaming according to the current
  ;; section of the org file, avoiding name clashes

  ;; required libraries
  (require 'dash)
  (require 'swiper)
  (require 's)

  (load-library "find-lisp")
  (when (boundp 'enable-personal-agenda)
    (when (is-linux-p)
      (setq org-agenda-files
            (find-lisp-find-files "~/nextcloud/12_tasks" "\.org$"))))
  (when (boundp 'enable-work-agenda)
    (when (is-windows-p)
      (setq org-agenda-files
            (find-lisp-find-files "~/nextcloud/03_documents/org/agenda/work" "\.org$"))))

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

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))

;; keymap for my personal.org file
(when (boundp 'enable-personal-agenda)
  (when (is-linux-p)
    (global-set-key (kbd "C-c p")
                    (lambda () (interactive) (find-file "~/nextcloud/12_tasks/personal.org")))))
(when (boundp 'enable-work-agenda)
  (when (is-windows-p)
    (global-set-key (kbd "C-c p")
                    (lambda () (interactive) (find-file "~/nextcloud/03_documents/org/agenda/work/work.org")))))

;; My details

(setq user-full-name "Andreas Zweili")
(setq user-mail-address "andreas@zweili.ch")

;; a function to toggle the splits
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-map "4" 'toggle-window-split)

(defun nebucatnetzer:split-window-below-and-move-cursor ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun nebucatnetzer:split-window-right-and-move-cursor ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'nebucatnetzer:split-window-below-and-move-cursor)
(global-set-key (kbd "C-x 3") 'nebucatnetzer:split-window-right-and-move-cursor)

;; Spaces instead of TABs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; keymap for buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; kill THIS buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; copy the complete buffer to the clipboard
(defun copy-all ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(global-set-key (kbd "C-S-c") 'copy-all)

;; keybinding for new frame
(global-set-key (kbd "C-x N") 'make-frame)

;; switch to frame
(global-set-key (kbd "C-x O") 'other-frame)

;; kill frame
(global-set-key (kbd "C-x K") 'delete-frame)

;; enable hippie expand on M-Space
(global-set-key "\M- " 'hippie-expand)

(defun switch-to-minibuffer () "Switch to minibuffer window."
       (interactive) (if (active-minibuffer-window)
                         (select-window
                          (active-minibuffer-window)) (error "Minibuffer is not active")))

(bind-key "M-m" 'switch-to-minibuffer)

;; file encodings
(prefer-coding-system 'utf-8-unix)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; hide temporary buffers
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-filter-by-name "^[^*]")))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . python-mode)
                 (mode . web-mode)
                 (mode . php-mode)
                 (mode . csharp-mode)
                 (mode . javascript-mode)
                 (mode . sql-mode)
                 (mode . powershell-mode)
                 (mode . nix-mode)
                 (mode . yaml-mode)
                 (mode . emacs-lisp-mode)))
               ;; etc
               ("Dired"
                (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; initial buffers should use text-mode
(setq-default major-mode 'text-mode)

                                        ; Calender should start on Monday
(setq calendar-week-start-day 1)

;; insert only one space after a period
(setq sentence-end-double-space nil)

                                        ; Matches parentheses and such in every mode
(show-paren-mode 1)

;; pair parentheses
(electric-pair-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq ad-redefinition-action 'accept)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-auto-revert-mode t)

(setq-default fill-column 79)

(setq column-number-mode 1)

(when (boundp 'enable-emojis)
  (when (is-linux-p)
    (set-fontset-font t nil "Symbola" nil 'prepend)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq
         (current-buffer)
         (buffer-list))))

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%d.%m.%Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-iso-date ()
  "Insert the current date in the ISO format."
  (interactive)
  (let ((format "%Y-%m-%d")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-full-date ()
  "Insert the current date, write out the day and month name."
  (interactive)
  (let ((format "%A, %d. %B %Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))
(defun generic-setup ()
  ;; turn off `linum-mode' when there are more than 5000 lines
  (if (buffer-too-big-p) (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook 'generic-setup)
(add-hook 'text-mode-hook 'generic-setup)

(setq history-delete-duplicates t)

(xterm-mouse-mode 1)

(add-hook 'text-mode-hook (lambda () (electric-indent-local-mode -1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq pcomplete-cycle-completions nil)))

(setq tramp-default-method "ssh")

(when (boundp 'enable-auctex)
  (defun LaTeX-collapse-table ()
    (interactive)
    (save-excursion
      (LaTeX-mark-environment)
      (while (re-search-forward "[[:space:]]+\\(&\\|\\\\\\\\\\)" (region-end) t)
        (replace-match " \\1"))))

  (defun LaTeX-align-environment (arg)
    (interactive "P")
    (if arg
        (LaTeX-collapse-table)
      (save-excursion
        (LaTeX-mark-environment)
        (align (region-beginning) (region-end))))))

(when (boundp 'enable-auctex)
  (defcustom LaTeX-inhibited-auto-fill-environments
    '("tabular" "tikzpicture") "For which LaTeX environments not to run auto-fill.")

  (defun LaTeX-limited-auto-fill ()
    (let ((environment (LaTeX-current-environment)))
      (when (not (member environment LaTeX-inhibited-auto-fill-environments))
        (do-auto-fill)))))

(when (boundp 'enable-auctex)
  (global-set-key (kbd "C-c f") 'LaTeX-align-environment)
  (setq auto-fill-function 'LaTeX-limited-auto-fill))

(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches "-alh")

;; keymap for dired
(global-set-key (kbd "C-c d") 'dired-jump)

(bind-keys :map dired-mode-map ("q" . az-kill-dired-buffers))

;;a function to kill all dired buffers
(defun az-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(use-package dired-hide-dotfiles
  :ensure t
  :init
  (defun my-dired-mode-hook ()
    "My `dired' mode hook."
    ;; To hide dot-files by default
    (dired-hide-dotfiles-mode)

    ;; To toggle hiding

    (add-hook 'dired-mode-hook #'my-dired-mode-hook))
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

;; -*- lexical-binding: t; -*-
(defun az-buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))

(defun az-center-buffer ()
  (interactive)
  (let ((margin-size (/ (- (frame-width) 80) 2)))
    (set-window-margins nil margin-size margin-size)))

(defun az-copy-all ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun az-generic-setup ()
  "turn off `linum-mode' when there are more than 5000 lines."
  (if (az-buffer-too-big-p) (display-line-numbers-mode -1)))

(defun az-kill-dired-buffers ()
  "Kill all buffers in Dired mode."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (eq major-mode 'dired-mode))
      (kill-buffer buffer))))

(defun az-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq
         (current-buffer)
         (buffer-list))))

(defun az-insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%d.%m.%Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun az-insert-iso-date ()
  "Insert the current date in the ISO format."
  (interactive)
  (let ((format "%Y-%m-%d")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun az-insert-full-date ()
  "Insert the current date, write out the day and month name."
  (interactive)
  (let ((format "%A, %d. %B %Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

;; taken from here: https://zck.org/emacs-move-file
(defun az-move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

;; Color theme
;; disable background in terminal
(defun az-on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun az-on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))

(defun az-open-notes ()
  "Create a notes perspective and open dired in the notes directory."
  (interactive)
  (persp-switch "notes")
  (dired denote-directory))

;; Taken from here: https://www.emacswiki.org/emacs/Replace-in-buffer
(defun az-replace-in-buffer ()
  "Search and replace given string in current buffer."
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setq curr-word (buffer-substring-no-properties (mark) (point)))
    (setq old-string (read-string "Replace: " curr-word))
    (setq new-string (read-string "With: " old-string))
    (query-replace old-string new-string nil (point-min) (point-max))))


(defun az-split-window-below-and-move-cursor ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun az-split-window-right-and-move-cursor ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun az-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive) (if (active-minibuffer-window)
                    (select-window
                     (active-minibuffer-window)) (error "Minibuffer is not active")))

(defun az-toggle-window-dedication ()
  "Toggle window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

;; a function to toggle the splits
(defun az-toggle-window-split ()
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

(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
  (eq system-type 'gnu/linux))

(defun is-windows-p ()
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)
      (eq system-type 'cygwin)))

(defun is-bsd-p ()
  (eq system-type 'gnu/kfreebsd))

(use-package emacs
  :config
  ;; Supress "ad-handle-definition: `tramp-read-passwd' got redefined" message at
  ;; start.
  (setopt ad-redefinition-action 'accept)

  (setopt auto-revert-use-notify nil)
  ;; Save temp files in the OS temp directory. Otherwise they clutter up the
  ;; current working directory
  (setopt auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
  ;; Save bookmarks right away
  (setopt bookmark-save-flag t)
  ;; Save temp files in the OS temp directory. Otherwise they clutter up the
  ;; current working directory
  (setopt backup-directory-alist
          `((".*" . ,temporary-file-directory)))

  (setopt column-number-mode t)
  ;; Prompt when quitting Emacs
  (setopt confirm-kill-emacs 'yes-or-no-p)
  ;; just create buffers don't ask
  (setopt confirm-nonexistent-file-or-buffer nil)
  ;; Send deleted files to the trash
  (setopt delete-by-moving-to-trash t)

  (setopt display-line-numbers-type t)
  (setopt ediff-split-window-function 'split-window-horizontally)
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Refresh buffers if the file changes on disk
  (setopt global-auto-revert-non-file-buffers t)

  (setopt history-delete-duplicates t)
  ;; just create buffers don't ask
  (setopt ido-create-new-buffer 'always)
  ;; Add groups to the buffer overview
  (setopt ibuffer-saved-filter-groups
          (quote (("default"
                   ("Notes" ;; all org-related buffers
                    (mode . markdown-mode)
                    (mode . org-mode))
                   ("Programming" ;; prog stuff not already in MyProjectX
                    (or
                     (mode . python-ts-mode)
                     (mode . web-mode)
                     (mode . php-ts-mode)
                     (mode . csharp-ts-mode)
                     (mode . javascript-mode)
                     (mode . sql-mode)
                     (mode . powershell-mode)
                     (mode . nix-mode)
                     (mode . yaml-ts-mode)
                     (mode . ansible-mode)
                     (mode . emacs-lisp-mode)))
                   ;; etc
                   ("Dired"
                    (mode . dired-mode))))))

  (setopt inhibit-compacting-font-caches t)
  ;; Disable splash screen
  (setopt inhibit-splash-screen t)
  ;; ispell settings
  (setopt ispell-program-name "hunspell")
  (setopt ispell-local-dictionary "en_GB")
  (setopt ispell-local-dictionary-alist
          '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
            ("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_CH") nil utf-8)))

  (setopt major-mode-remap-alist
          '(
            (bash-mode . bash-ts-mode)
            (csharp-mode . csharp-ts-mode)
            (css-mode . css-ts-mode)
            (go-mode . go-ts-mode)
            (java-mode . java-ts-mode)
            (js2-mode . js-ts-mode)
            (json-mode . json-ts-mode)
            (php-mode . php-ts-mode)
            (python-mode . python-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (yaml-mode . yaml-ts-mode)
            ))

  ;; switch focus to man page
  (setopt man-notify-method t)
  ;; disbale the bell
  (setopt ring-bell-function 'ignore)
  ;; insert only one space after a period
  (setopt sentence-end-double-space nil)
  (setopt sh-basic-offset 4)
  ;; disable tooltips
  (setopt tooltip-use-echo-area t)

  (setopt use-short-answers t)
  ;; My details
  (setopt user-full-name "Andreas Zweili")
  (setopt user-mail-address "andreas@zweili.ch")
  ;; always follow symlinks
  (setopt vc-follow-symlinks t)
  ;; use ripgrep or rg if possible
  (setopt xref-search-program (cond ((or (executable-find "ripgrep")
                                         (executable-find "rg")) 'ripgrep)
                                    ((executable-find "ugrep") 'ugrep) (t
                                                                        'grep)))

  (setopt browse-url-browser-function 'browse-url-generic
          browse-url-generic-program (getenv "DEFAULT_BROWSER"))

  (setq-default
   fill-column 79
   ;; Spaces instead of TABs
   indent-tabs-mode nil
   ;; initial buffers should use text-mode
   major-mode 'text-mode
   tab-width 4)

  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "   "
                  mode-line-misc-info
                  ))

  ;; required for autoloading packages on nixos
  (dolist (path load-path)
    (when (string-match-p "/nix/store/[a-z0-9]\\{32\\}-emacs-packages-deps.*" path)
      (dolist (autoload-file (directory-files path t "-autoloads.el"))
        (with-demoted-errors "init.el error: %s"
          (load autoload-file nil t)))))

  ;; Create a new window when there isn't one.
  ;; Taken from: https://karthinks.com/software/emacs-window-management-almanac/#double-duty
  (advice-add 'other-window :before
              (defun other-window-split-if-single (&rest _)
                "Split the frame if there is a single window."
                (when (one-window-p) (split-window-sensibly))))

  (az-on-frame-open (selected-frame))
  ;; pair parentheses
  (electric-pair-mode 1)
  ;; remap yes or no to y or n
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Refresh buffers if the file changes on disk
  (global-auto-revert-mode t)

  (global-display-line-numbers-mode)
  ;; Proper line wrapping
  (global-visual-line-mode 1)
  ;; disable menu and toolbar
  (menu-bar-mode -99)
  ;; file encodings
  (prefer-coding-system 'utf-8-unix)
  ;; ispell settings
  (setenv "DICTIONARY" "en_GB")
  ;; Matches parentheses and such in every mode
  (show-paren-mode 1)

  (toggle-frame-maximized)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  ;; enable mouse support in the terminal
  (xterm-mouse-mode 1)

  (when (boundp 'enable-scroll-bar)
    (scroll-bar-mode -1))
  ;; Disable fringe because I use visual-line-mode
  (when (boundp 'disable-fringe)
    (set-fringe-mode '(0 . 0)))
  (when (boundp 'enable-font)
    (set-face-attribute 'default nil
                        :family "0xProto"
                        :height 140
                        :weight 'normal
                        :width 'normal))
  (when (boundp 'enable-emojis)
    (when (is-linux-p)
      (set-fontset-font t nil "Symbola" nil 'prepend)))

  (add-hook 'after-make-frame-functions 'az-on-frame-open)
  :hook
  (
   ;; Remove whitespace when saving
   (before-save . whitespace-cleanup)

   (ibuffer-mode .
                 (lambda ()
                   (ibuffer-switch-to-saved-filter-groups "default")))
   ;; hide temporary buffers
   (ibuffer-mode .
                 (lambda ()
                   (ibuffer-filter-by-name "^[^*]")))
   ;; improve performance with large files (
   (prog-mode . az-generic-setup)
   (text-mode . az-generic-setup)
   ;; )
   ;; Enable line wrapping
   (text-mode  . turn-on-auto-fill)
   (window-setup . az-on-after-init))
  :bind
  (:map global-map
        ("C-x C-1" . delete-other-windows)
        ("C-x C-2" . az-split-window-below-and-move-cursor)
        ("C-x C-3" . az-split-window-right-and-move-cursor)
        ("C-x C-4" . az-toggle-window-split)
        ("C-x C-0" . kill-buffer-and-window)
        ;; kill THIS buffer
        ("C-x C-k" . kill-current-buffer)
        ("C-S-c" . az-copy-all)
        ;; keybinding for new frame
        ("C-x N" . make-frame)
        ;; switch to frame
        ("C-x O" . other-frame)
        ;; kill frame
        ("C-x K" . delete-frame)
        ;; keymap for dired
        ("C-x d" . dired-jump)
        ("M-m" . az-switch-to-minibuffer)
        ("<f5>" . az-open-notes)
        ))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package dired
  :ensure nil
  :init
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x")))
  (with-eval-after-load 'locate
    (define-key locate-mode-map (kbd "SPC") 'god-execute-with-current-bindings))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-listing-switches "-Ahl --group-directories-first")
  (setopt dired-auto-revert-buffer t))

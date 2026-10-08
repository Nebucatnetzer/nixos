;; -*- lexical-binding: t; -*-
;; Personal commands. Each package adds its keys with :bind (:map az-map ...).
;; Keys are C-<letter>, so god-mode reaches C-l C-a as SPC l a.
(defvar-keymap az-map
  :doc "Personal command map on C-l."
  "C-l" #'recenter-top-bottom)
(keymap-global-set "C-l" az-map)

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

  ;; Sync GUI environment variables when a graphical client connects
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (let ((display (frame-parameter nil 'display)))
                ;; Only set the environment if it's a graphical frame (not terminal)
                (when display
                  (setenv "DISPLAY" display)
                  ;; If you use Wayland, this ensures modern browsers route correctly
                  (setenv "WAYLAND_DISPLAY" display)))))

  (setopt history-delete-duplicates t)
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
                     (mode . php-mode)
                     (mode . csharp-ts-mode)
                     (mode . javascript-mode)
                     (mode . sql-mode)
                     (mode . powershell-mode)
                     (mode . nix-ts-mode)
                     (mode . yaml-ts-mode)
                     (mode . ansible-mode)
                     (mode . emacs-lisp-mode)))
                   ;; etc
                   ("Dired"
                    (mode . dired-mode))))))

  (setopt inhibit-compacting-font-caches t)
  ;; Disable splash screen
  (setopt inhibit-splash-screen t)

  ;; switch focus to man page
  (setopt man-notify-method t)
  ;; disbale the bell
  (setopt ring-bell-function 'ignore)
  ;; insert only one space after a period
  (setopt sentence-end-double-space nil)
  (setopt sh-basic-offset 4)

  (setopt use-short-answers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Allow minibuffer commands inside the minibuffer
  (setopt enable-recursive-minibuffers t)
  (setopt read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t)
  ;; Hide commands in M-x which do not work in the current mode
  (setopt read-extended-command-predicate #'command-completion-default-include-p)

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
          browse-url-secondary-browser-function 'browse-url-generic
          browse-url-generic-program (getenv "DEFAULT_BROWSER"))

  (global-set-key [remap keyboard-quit] #'az-keyboard-quit)

  (setq-default
   fill-column 88
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

  ;; Create a new window when there isn't one.
  ;; Taken from: https://karthinks.com/software/emacs-window-management-almanac/#double-duty
  (advice-add 'other-window :before
              (defun other-window-split-if-single (&rest _)
                "Split the frame if there is a single window."
                (when (one-window-p) (split-window-sensibly))))

  ;; pair parentheses
  (electric-pair-mode 1)
  ;; Refresh buffers if the file changes on disk
  (global-auto-revert-mode t)

  ;; Only enable line-numbers in specific modes
  (defvar az/line-numbers-exempt-modes '(org-mode markdown-mode)
    "Major modes that never get line numbers.
  Needed because Org derives from `text-mode', so it would otherwise
  inherit them from the hook below.")

  (defun az/enable-line-numbers ()
    "Turn on line numbers unless the major mode is exempt."
    (unless (apply #'derived-mode-p az/line-numbers-exempt-modes)
      (display-line-numbers-mode 1)))

  ;; Opt in per mode family instead of globally: prose read in a centred
  ;; Olivetti column has no use for a number gutter.
  (dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
    (add-hook hook #'az/enable-line-numbers))


  ;; Proper line wrapping
  (global-visual-line-mode 1)
  ;; disable menu and toolbar
  (menu-bar-mode -1)
  ;; file encodings
  (prefer-coding-system 'utf-8-unix)

  (tool-bar-mode -1)
  (tooltip-mode -1)
  ;; enable mouse support in the terminal
  (xterm-mouse-mode 1)

  (when (bound-and-true-p disable-scroll-bar)
    (scroll-bar-mode -1))
  ;; Disable fringe because I use visual-line-mode
  (when (and (bound-and-true-p disable-fringe) (fboundp 'set-fringe-mode))
    (set-fringe-mode '(0 . 0)))
  (when (bound-and-true-p enable-font)
    (set-face-attribute 'default nil
                        :family "Source Code Pro"
                        :height 140
                        :weight 'normal
                        :width 'normal))
  (when (bound-and-true-p enable-emojis)
    (when (is-linux-p)
      (set-fontset-font t nil "Symbola" nil 'prepend)))

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
   ;; Enable line wrapping
   (text-mode  . turn-on-auto-fill))
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
        ;; kill frame
        ("C-x K" . delete-frame)
        ;; keymap for dired
        ("C-x d" . dired-jump)
        ("M-m" . az-switch-to-minibuffer)
        ))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package dired
  :init
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x")))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-listing-switches "-Ahl --group-directories-first")
  (setopt dired-auto-revert-buffer t))

;; Skip gnu-elpa-keyring-update in read-only Nix store configs
;; (use-package gnu-elpa-keyring-update)

;; browse-url sets up the GUI display before it opens a URL, and that fails
;; in terminal frames. Call the URL handler directly when there is no GUI.
(defun az-browse-url-in-terminal (orig-fn url &rest args)
  "Call ORIG-FN in graphical frames, else dispatch URL to its handler directly."
  (if (display-graphic-p)
      (apply orig-fn url args)
    (let ((handler (or (browse-url-select-handler url)
                       browse-url-browser-function)))
      (apply handler url args))))
(advice-add 'browse-url :around #'az-browse-url-in-terminal)

;; Clipboard for terminal frames: copy through OSC 52 escape sequences so
;; yanks reach the host clipboard over any terminal (Wayland, X, SSH) without
;; an external helper. Replaces xclip, which was X11-only and dead on Wayland.
;; Paste still comes from the terminal emulator's own paste binding.
(unless (az-wsl-p)
  (when (bound-and-true-p enable-clipetty)
    (use-package clipetty
      :config
      (global-clipetty-mode 1))))

;; Clipboard in WSL — win32yank rather than the OSC 52 path above. Emacs is
;; run here in the terminal, where OSC 52 would only cover copy; win32yank
;; shells out to the Windows clipboard for both copy and paste, so yanking
;; Windows-copied text into Emacs keeps working.
(when (az-wsl-p)
  (setq interprogram-cut-function
        (lambda (text &optional _push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "win32yank-cut" nil "win32yank.exe" "-i" "--crlf")))
              (process-send-string proc text)
              (process-send-eof proc)))))

  (setq interprogram-paste-function
        (lambda ()
          (let ((text (shell-command-to-string "win32yank.exe -o --lf")))
            ;; If the text is empty OR it perfectly matches the top of the kill-ring,
            ;; return nil. Otherwise, return the new text.
            (if (or (string= text "")
                    (and kill-ring (string= text (car kill-ring))))
                nil
              text)))))

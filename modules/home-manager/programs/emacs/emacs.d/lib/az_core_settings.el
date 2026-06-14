;; -*- lexical-binding: t; -*-
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
                     (mode . php-mode)
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

  (setopt major-mode-remap-alist
          '(
            (bash-mode . bash-ts-mode)
            (csharp-mode . csharp-ts-mode)
            (css-mode . css-ts-mode)
            (go-mode . go-ts-mode)
            (java-mode . java-ts-mode)
            (js2-mode . js-ts-mode)
            (json-mode . json-ts-mode)
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
  ;; Matches parentheses and such in every mode
  (show-paren-mode 1)

  (tool-bar-mode -1)
  (tooltip-mode -1)
  ;; enable mouse support in the terminal
  (xterm-mouse-mode 1)

  (when (boundp 'enable-scroll-bar)
    (scroll-bar-mode -1))
  ;; Disable fringe because I use visual-line-mode
  (when (and (boundp 'disable-fringe) (fboundp 'set-fringe-mode))
    (set-fringe-mode '(0 . 0)))
  (when (boundp 'enable-font)
    (set-face-attribute 'default nil
                        :family "Source Code Pro"
                        :height 140
                        :weight 'normal
                        :width 'normal))
  (when (boundp 'enable-emojis)
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
   ;; improve performance with large files (
   (prog-mode . az-generic-setup)
   (text-mode . az-generic-setup)
   ;; )
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
        ("<f5>" . az-open-notes)
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

;; Clipboard for terminal frames: copy through OSC 52 escape sequences so
;; yanks reach the host clipboard over any terminal (Wayland, X, SSH) without
;; an external helper. Replaces xclip, which was X11-only and dead on Wayland.
;; Paste still comes from the terminal emulator's own paste binding.
(unless (file-exists-p "/etc/wsl.conf")
  (when (boundp 'enable-clipetty)
    (use-package clipetty
      :config
      (global-clipetty-mode 1))))

;; Clipboard in WSL — win32yank rather than the OSC 52 path above. Emacs is
;; run here in the terminal, where OSC 52 would only cover copy; win32yank
;; shells out to the Windows clipboard for both copy and paste, so yanking
;; Windows-copied text into Emacs keeps working.
(when (file-exists-p "/etc/wsl.conf")
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

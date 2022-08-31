;; always follow symlinks
(setq vc-follow-symlinks t)

;; Spaces instead of TABs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Enable hippie expand on M-Space. It helps with path completion and more
(global-set-key "\M- " 'hippie-expand)

;; file encodings
(prefer-coding-system 'utf-8-unix)

;; Save temp files in the OS temp directory. Otherwise they clutter up the
;; current working directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; initial buffers should use text-mode
(setq-default major-mode 'text-mode)

;; insert only one space after a period
(setq sentence-end-double-space nil)

;; Enable line wrapping
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; line length
(setq-default fill-column 79)

;; Supress "ad-handle-definition: `tramp-read-passwd' got redefined" message at
;; start.
(setq ad-redefinition-action 'accept)

;; Remove whitespace when saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Refresh buffers if the file changes on disk
(global-auto-revert-mode t)

(setq history-delete-duplicates t)

;; enable mouse support in the terminal
(xterm-mouse-mode 1)

;; Disable electric indents
(add-hook 'text-mode-hook (lambda () (electric-indent-local-mode -1)))

;; Make the tab completion behave like in Bash.
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq pcomplete-cycle-completions nil)))

;; For better performance use "ssh" instead of "scp"
(setq tramp-default-method "ssh")

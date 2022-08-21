(use-package bug-hunter
  :defer t
  :ensure t)

(use-package direnv
  :config
  (direnv-mode))

;; My details

(setq user-full-name "Andreas Zweili")
(setq user-mail-address "andreas@zweili.ch")

;; Spaces instead of TABs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; enable hippie expand on M-Space
(global-set-key "\M- " 'hippie-expand)

;; file encodings
(prefer-coding-system 'utf-8-unix)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; initial buffers should use text-mode
(setq-default major-mode 'text-mode)

;; Calender should start on Monday
(setq calendar-week-start-day 1)

;; insert only one space after a period
(setq sentence-end-double-space nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq ad-redefinition-action 'accept)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-auto-revert-mode t)

(setq-default fill-column 79)

(setq history-delete-duplicates t)

(xterm-mouse-mode 1)

(add-hook 'text-mode-hook (lambda () (electric-indent-local-mode -1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq pcomplete-cycle-completions nil)))

(setq tramp-default-method "ssh")

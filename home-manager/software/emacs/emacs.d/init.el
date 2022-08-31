;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; My details
(setq user-full-name "Andreas Zweili")
(setq user-mail-address "andreas@zweili.ch")

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

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
(defun config-path (config)
  (setq emacs-config-dir "~/.nixos/home-manager/software/emacs/emacs.d/lib/")
  (concat emacs-config-dir config))

;; load specific config files
;; basic settings
(load-file "~/.emacs.d/variables.el")
(load-file (config-path "detect_os.el"))
(load-file (config-path "dired.el"))
(load-file (config-path "packages.el"))
(load-file (config-path "theme.el"))
(load-file (config-path "ui.el"))
(load-file (config-path "utils.el"))
(load-file (config-path "window_split.el"))

;; keybindings
(load-file (config-path "evil.el"))
(load-file (config-path "hydra.el"))
(load-file (config-path "keybindings.el"))

;; note taking and writing
(load-file (config-path "deft.el"))
(load-file (config-path "latex.el"))
(load-file (config-path "langtool.el"))
(load-file (config-path "markdown.el"))
(load-file (config-path "move_text.el"))
(load-file (config-path "org_mode.el"))
(load-file (config-path "org_insert.el"))
(load-file (config-path "pdf_tools.el"))
(load-file (config-path "spellcheck.el"))

;; programming
(load-file (config-path "bug_hunter.el"))
(load-file (config-path "company.el"))
(load-file (config-path "direnv.el"))
(load-file (config-path "format_all.el"))
(load-file (config-path "magit.el"))
(load-file (config-path "powershell.el"))
(load-file (config-path "python.el"))
(load-file (config-path "webmode.el"))
(load-file (config-path "yaml.el"))

;; various third party
(load-file (config-path "counsel.el"))
(load-file (config-path "eyebrowse.el"))
(load-file (config-path "keyring_update.el"))
(load-file (config-path "manuals.el"))
(load-file (config-path "mu4e.el"))
(load-file (config-path "treemacs.el"))
(load-file (config-path "vterm.el"))
(load-file (config-path "yasnippet.el"))

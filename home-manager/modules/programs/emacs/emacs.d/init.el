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

(defun modules-path (config)
  (setq emacs-config-dir "~/.nixos/home-manager/software/emacs/emacs.d/lib/")
  (concat emacs-config-dir config))

;; load config files
(load-file "~/.emacs.d/variables.el")
(load-file "~/.nixos/home-manager/software/emacs/emacs.d/modules.el")

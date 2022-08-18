;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq vc-follow-symlinks t);; always follow symlinks

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(require 'ob-tangle)
(defun config-path (config)
  (setq emacs-config-dir "~/.nixos/home-manager/software/emacs/emacs.d/")
  (concat emacs-config-dir config))


(load-file (config-path "loader.el"))

;; -*- lexical-binding: t; -*-
;; Packages come from Nix. Emacs activates them before this file runs.
(package-initialize)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(defun modules-path (config)
  "Return the path of CONFIG in the lib directory."
  (concat "~/.nixos/modules/home-manager/programs/emacs/emacs.d/lib/" config))

;; load config files
(load-file "~/.emacs.d/variables.el")
(load-file "~/.nixos/modules/home-manager/programs/emacs/emacs.d/modules.el")

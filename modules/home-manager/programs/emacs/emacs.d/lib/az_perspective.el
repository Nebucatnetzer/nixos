;; -*- lexical-binding: t; -*-
(use-package perspective
  :after consult
  :bind
  (("C-x b" . persp-ibuffer)         ; or use a nicer switcher, see below
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))  ; pick your own prefix key here
  :config
  (consult-customize consult-source-buffer :hidden t :default nil)
  ;; perspective binds to mouse click instead of release which then sometimes causes the org clock drawer to fire.
  ;; Rebinding it to mouse release.
  (keymap-unset persp-mode-line-map "<mode-line> <down-mouse-1>" t)
  (keymap-set persp-mode-line-map "<mode-line> <mouse-1>" #'persp-mode-line-click)

  (add-to-list 'consult-buffer-sources persp-consult-source)
  :init
  (setopt persp-state-default-file "~/.emacs.d/persp-session")
  (persp-mode))

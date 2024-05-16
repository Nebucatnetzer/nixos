;; -*- lexical-binding: t; -*-
(use-package perspective
  :after consult
  :bind
  (("C-x C-b" . persp-ibuffer)         ; or use a nicer switcher, see below
   ;;("C-x b" . persp-ivy-switch-buffer)
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))  ; pick your own prefix key here
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :init
  (define-key evil-normal-state-map "gt" 'persp-next)
  (define-key evil-normal-state-map "gT" 'persp-prev)
  (define-key evil-normal-state-map "gr" 'persp-rename)
  (define-key evil-normal-state-map "gc" 'persp-kill)
  (setq persp-state-default-file "~/.emacs.d/persp-session")
  (persp-mode))

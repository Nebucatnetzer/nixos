(use-package perspective
  :bind
  (("C-x C-b" . persp-ibuffer)         ; or use a nicer switcher, see below
   ("C-x b" . persp-ivy-switch-buffer)
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))  ; pick your own prefix key here
  :init
  (define-key evil-normal-state-map "gt" 'persp-next)
  (define-key evil-normal-state-map "gT" 'persp-prev)
  (define-key evil-normal-state-map "gr" 'persp-rename)
  (define-key evil-normal-state-map "gc" 'persp-kill)
  (persp-mode))

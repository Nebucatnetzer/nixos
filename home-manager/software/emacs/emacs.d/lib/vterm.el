(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (evil-set-initial-state 'vterm-mode 'emacs))

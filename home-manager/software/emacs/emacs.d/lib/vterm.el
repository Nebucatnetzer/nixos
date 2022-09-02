(use-package vterm
  :ensure t
  :config
  (global-set-key (kbd "M-RET") 'vterm)
  (setq vterm-kill-buffer-on-exit t)
  (evil-set-initial-state 'vterm-mode 'emacs))

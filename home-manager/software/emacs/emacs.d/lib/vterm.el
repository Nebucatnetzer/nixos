(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-disable-bold t)
  (global-set-key (kbd "M-RET") 'vterm)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package multi-vterm
  :ensure t
  :config
  (global-set-key (kbd "M-S RET") 'multi-vterm))

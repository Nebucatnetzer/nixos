(use-package vterm
  :if (is-linux-p)
  :bind
  ([f2] . vterm)
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-disable-bold t)
  (evil-set-initial-state 'vterm-mode 'emacs))

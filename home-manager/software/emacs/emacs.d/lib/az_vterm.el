(when (is-linux-p)
  (use-package vterm
    :config
    (setq vterm-kill-buffer-on-exit t
          vterm-disable-bold t)
    (evil-set-initial-state 'vterm-mode 'emacs))

  (use-package multi-vterm)

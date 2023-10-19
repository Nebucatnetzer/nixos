(use-package vterm
  :if (is-linux-p)
  :bind
  ([f2] . projectile-run-vterm)
  :config
  (add-to-list 'vterm-tramp-shells '("ssh" "/usr/bin/env bash"))
  (add-to-list 'vterm-tramp-shells '("sshx" "/usr/bin/env bash"))
  (setq vterm-kill-buffer-on-exit t
        vterm-shell "/run/current-system/sw/bin/bash"
        vterm-disable-bold t)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode -1))))

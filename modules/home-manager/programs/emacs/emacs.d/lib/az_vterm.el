;; -*- lexical-binding: t; -*-
(use-package vterm
  :if (is-linux-p)
  :bind
  ([f2] . projectile-run-vterm)
  :config
  (add-to-list 'vterm-tramp-shells '("ssh" "/usr/bin/env bash"))
  (add-to-list 'vterm-tramp-shells '("sshx" "/usr/bin/env bash"))
  (setopt vterm-kill-buffer-on-exit t
          vterm-shell "/usr/bin/env bash"
          vterm-disable-bold t)
  (add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode -1))))

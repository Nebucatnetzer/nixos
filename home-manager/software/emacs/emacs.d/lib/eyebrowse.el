(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c M-e"))
  (global-unset-key (kbd "C-c C-w"))
  (define-key evil-normal-state-map "gt" 'eyebrowse-next-window-config)
  (define-key evil-normal-state-map "gT" 'eyebrowse-prev-window-config)
  (define-key evil-normal-state-map "gr" 'eyebrowse-rename-window-config)
  (define-key evil-normal-state-map "gc" 'eyebrowse-close-window-config)
  :config
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-switch-back-and-forth t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode 1))

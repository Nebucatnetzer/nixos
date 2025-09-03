;; -*- lexical-binding: t; -*-
(use-package hyperbole
  :init
  (setopt hywiki-directory "~/.emacs.d/hywiki")
  :config
  (hyperbole-mode 1)
  (define-key hyperbole-mode-map (kbd "M-o"  ) nil)
  )

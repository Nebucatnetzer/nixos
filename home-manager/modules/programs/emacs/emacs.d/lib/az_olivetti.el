;; -*- lexical-binding: t; -*-
(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :init
  (setq olivetti-body-width 120))

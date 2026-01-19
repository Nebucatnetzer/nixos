;; -*- lexical-binding: t; -*-
(use-package treemacs
  :bind ("<f12>" . treemacs-display-current-project-exclusively)
  :config
  (progn
    (use-package treemacs-evil
      :demand t)))

;; -*- lexical-binding: t; -*-
(use-package treemacs
  :bind ("<f12>" . treemacs-display-current-project-exclusively))

(use-package treemacs-evil
  :after (treemacs evil))

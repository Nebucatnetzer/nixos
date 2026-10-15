;; -*- lexical-binding: t; -*-
(use-package ghostel
  :if (az-is-linux-p)
  :bind ([f2] . ghostel-project)
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))

(use-package evil-ghostel
  :if (az-is-linux-p)
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

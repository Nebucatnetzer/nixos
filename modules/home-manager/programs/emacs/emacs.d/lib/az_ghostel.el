;; -*- lexical-binding: t; -*-
(use-package ghostel
  :if (is-linux-p)
  :bind ([f2] . ghostel-project)
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-hook 'ghostel-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package evil-ghostel
  :if (is-linux-p)
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

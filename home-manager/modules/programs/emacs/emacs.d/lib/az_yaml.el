;; -*- lexical-binding: t; -*-
(use-package ansible
  :after yaml-ts-mode
  :config (add-hook 'yaml-ts-mode-hook '(lambda () (ansible-mode 1))))

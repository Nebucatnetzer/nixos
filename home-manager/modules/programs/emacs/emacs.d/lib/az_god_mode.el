;; -*- lexical-binding: t; -*-
;; https://github.com/emacsorphanage/god-mode
(use-package god-mode
  :init
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (evil-define-key 'normal global-map (kbd "SPC") 'god-execute-with-current-bindings)
  (setq god-mode-enable-function-key-translation nil))

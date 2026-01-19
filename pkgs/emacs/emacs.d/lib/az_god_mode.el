;; -*- lexical-binding: t; -*-
;; https://github.com/emacsorphanage/god-mode
(use-package god-mode
  :init
  (setopt god-exempt-major-modes nil
          god-exempt-predicates nil
          god-mode-enable-function-key-translation nil)
  (evil-define-key 'normal global-map (kbd "SPC") 'god-execute-with-current-bindings))

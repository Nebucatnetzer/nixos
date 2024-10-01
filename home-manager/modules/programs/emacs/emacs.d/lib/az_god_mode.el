;; -*- lexical-binding: t; -*-
;; https://github.com/emacsorphanage/god-mode
(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil))

(use-package evil-god-state
  :commands evil-execute-in-god-state
  :init
  (evil-define-key 'normal global-map (kbd "SPC") 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

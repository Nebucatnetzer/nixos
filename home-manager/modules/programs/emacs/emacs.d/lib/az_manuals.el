;; -*- lexical-binding: t; -*-
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h s" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-<tab>" . backward-button)
         :map helpful-mode-map
         ("M-?" . helpful-at-point)
         :map emacs-lisp-mode-map
         ("M-?" . helpful-at-point)
         :map lisp-interaction-mode-map  ; Scratch buffer
         ("M-?" . helpful-at-point)))

;; which key is a package to show which keys can be pressed
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

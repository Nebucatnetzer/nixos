(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h s" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-c h f" . helpful-function)
         ("C-c h v" . helpful-variable)
         ("C-c h c" . helpful-command)
         ("C-c h m" . helpful-macro)
         ("<C-tab>" . backward-button)
         :map helpful-mode-map
         ("M-?" . helpful-at-point)
         :map emacs-lisp-mode-map
         ("M-?" . helpful-at-point)
         :map lisp-interaction-mode-map  ; Scratch buffer
         ("M-?" . helpful-at-point)))

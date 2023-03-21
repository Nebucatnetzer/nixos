(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(yaml-mode . ("ansible-language-server" "--stdio")))
  ;; (add-hook 'yaml-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook #'eglot-ensure))

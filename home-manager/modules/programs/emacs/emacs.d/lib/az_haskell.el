;; -*- lexical-binding: t; -*-
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  (add-hook 'haskell-literate-mode-hook #'lsp-deferred))

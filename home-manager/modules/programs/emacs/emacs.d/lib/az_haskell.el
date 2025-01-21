;; -*- lexical-binding: t; -*-
(use-package haskell-mode
  :hook
  (haskell-mode . eglot-ensure)
  (haskell-literate-mode . eglot-ensure))

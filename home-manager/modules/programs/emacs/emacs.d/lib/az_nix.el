;; -*- lexical-binding: t; -*-
(use-package nix-mode
  :hook (nix-ts-mode . lsp-deferred))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

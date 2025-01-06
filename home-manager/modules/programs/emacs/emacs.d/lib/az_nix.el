;; -*- lexical-binding: t; -*-
(use-package nix-mode
  :hook (nix-mode . eglot-ensure))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

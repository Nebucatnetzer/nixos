(use-package lsp-nix
  :after (lsp-mode)
  :demand t
  :ensure nil
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred))

{
  pkgs,
  unstable-pkgs,
  lib,
  # Language servers, formatters and linters that Emacs shells out to per
  # language. Disabled for the portable terminal az-emacs build, which ships
  # only the language-agnostic tooling the config uses everywhere.
  includeLanguageTools ? true,
}:
[
  pkgs.direnv # envrc-global-mode shells out to this for per-project tooling
  pkgs.emacs-lsp-booster # eglot-booster shells out to this
  pkgs.fd
  pkgs.hurl # required for hurl-mode
  # format-all runs this on every .nix save; ~5 MB on top of pandoc's closure
  pkgs.nixfmt
  pkgs.pandoc # org mode
  pkgs.ripgrep
  pkgs.shellcheck # shell script linter
  pkgs.shfmt # formatting bash scripts
  pkgs.silver-searcher
]
++ lib.optionals includeLanguageTools [
  pkgs.deadnix
  pkgs.multimarkdown
  pkgs.nixd # Nix language server
  unstable-pkgs.prettier # formatting files
  pkgs.statix
]

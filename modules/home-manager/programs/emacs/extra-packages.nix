{
  pkgs,
  unstable-pkgs,
  epkgs,
  lib,
  includeGuiPackages ? false,
  # Email (mu4e), note-taking (denote), citation/languagetool tooling. Disabled for the
  # portable terminal az-emacs build, which is scoped to basic coding plus the
  # occasional org note.
  includeExtendedPackages ? true,
}:
[
  epkgs.ace-window
  unstable-pkgs.emacs.pkgs.alabaster-themes
  epkgs.ansible
  epkgs.avy
  epkgs.clipetty
  epkgs.company
  epkgs.consult
  epkgs.consult-project-extra
  epkgs.eglot-booster
  epkgs.embark
  epkgs.embark-consult
  epkgs.envrc
  epkgs.evil
  epkgs.evil-collection
  epkgs.evil-surround
  epkgs.flymake-ansible-lint
  epkgs.flymake-collection
  epkgs.format-all
  unstable-pkgs.emacs.pkgs.ghostel
  (pkgs.callPackage ./packages/evil-ghostel {
    inherit (epkgs) melpaBuild evil;
    ghostel = unstable-pkgs.emacs.pkgs.ghostel;
  })
  epkgs.god-mode
  epkgs.haskell-mode
  epkgs.helpful
  epkgs.highlight-indent-guides
  epkgs.htmlize
  (pkgs.callPackage ./packages/hurl-mode {
    inherit (epkgs) melpaBuild;
  })
  epkgs.jq-mode
  epkgs.magit
  epkgs.marginalia
  epkgs.markdown-mode
  epkgs.nix-ts-mode
  epkgs.olivetti
  epkgs.orderless
  epkgs.org-contrib
  epkgs.ox-pandoc
  epkgs.perspective
  epkgs.powershell
  epkgs.python-pytest
  epkgs.rainbow-delimiters
  epkgs.treemacs
  epkgs.treemacs-evil
  epkgs.treesit-auto
  epkgs.typst-ts-mode
  unstable-pkgs.emacs.pkgs.treesit-grammars.with-all-grammars
  epkgs.ultra-scroll
  epkgs.vertico
  epkgs.vundo
  epkgs.web-mode
  epkgs.wgrep
  epkgs.yaml-mode
  epkgs.yasnippet-snippets
]
++ lib.optionals includeGuiPackages [
  epkgs.pdf-tools
]
++ lib.optionals includeExtendedPackages [
  # AI code completion (requires an Infomaniak API token)
  epkgs.minuet
  # citations
  epkgs.citeproc
  epkgs.citar
  epkgs.citar-denote
  epkgs.citar-embark
  epkgs.parsebib
  # note-taking (denote)
  unstable-pkgs.emacs.pkgs.consult-denote
  unstable-pkgs.emacs.pkgs.denote
  unstable-pkgs.emacs.pkgs.denote-journal
  unstable-pkgs.emacs.pkgs.denote-org
  # email
  epkgs.mu4e
  pkgs.mu # needed for mailing
  # languagetool prose linting
  epkgs.flymake-languagetool
]

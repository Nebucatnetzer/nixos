{
  pkgs,
  writeShellApplication,
  consult-denote,
  denote,
  denote-journal,
  denote-org,
}:
let
  waylandEmacs = pkgs.emacs-pgtk;
  emacsWithPackages = (pkgs.emacsPackagesFor waylandEmacs).emacsWithPackages;
  myEmacs = emacsWithPackages (epkgs: [
    epkgs.ace-window
    epkgs.ag
    epkgs.ansible
    epkgs.avy
    epkgs.bind-key
    epkgs.cape
    epkgs.cfrs
    epkgs.citeproc
    epkgs.consult
    consult-denote
    epkgs.consult-projectile
    epkgs.corfu
    epkgs.corfu-terminal
    epkgs.dap-mode
    denote
    denote-journal
    denote-org
    epkgs.elisp-refs
    epkgs.embark
    epkgs.embark-consult
    epkgs.envrc
    epkgs.epl
    epkgs.evil
    epkgs.evil-collection
    epkgs.evil-surround
    epkgs.f
    epkgs.flymake-ansible-lint
    epkgs.flymake-collection
    epkgs.flymake-flycheck
    epkgs.flymake-ruff
    epkgs.flymake-languagetool
    epkgs.flymake-shellcheck
    epkgs.format-all
    epkgs.general
    epkgs.god-mode
    epkgs.haskell-mode
    epkgs.helpful
    epkgs.highlight-indent-guides
    epkgs.ht
    epkgs.htmlize
    (pkgs.callPackage ./packages/hurl-mode {
      melpaBuild = epkgs.melpaBuild;
    })
    epkgs.hydra
    epkgs.hyperbole
    epkgs.jq-mode
    epkgs.know-your-http-well
    epkgs.lsp-haskell
    epkgs.lv
    epkgs.magit
    epkgs.makey
    epkgs.marginalia
    epkgs.markdown-mode
    epkgs.mu4e
    epkgs.nix-mode
    epkgs.nix-ts-mode
    epkgs.olivetti
    epkgs.orderless
    epkgs.org-contrib
    epkgs.ox-pandoc
    epkgs.parsebib
    epkgs.pdf-tools
    epkgs.perspective
    epkgs.pfuture
    epkgs.php-mode
    epkgs.pkg-info
    epkgs.posframe
    epkgs.powershell
    epkgs.projectile
    epkgs.projectile-ripgrep
    epkgs.python-mode
    epkgs.python-pytest
    epkgs.queue
    epkgs.rainbow-delimiters
    epkgs.ripgrep
    epkgs.smooth-scrolling
    epkgs.solarized-theme
    epkgs.string-inflection
    epkgs.swiper
    epkgs.system-packages
    epkgs.treemacs
    epkgs.treemacs-evil
    epkgs.treesit-grammars.with-all-grammars
    epkgs.vertico
    epkgs.vterm
    epkgs.vundo
    epkgs.web-completion-data
    epkgs.web-mode
    epkgs.wgrep
    epkgs.which-key
    epkgs.xclip
    epkgs.yaml-mode
    epkgs.yasnippet-snippets
    pkgs.mu # needed for mailing
  ]);
in
writeShellApplication {
  name = "emacs";
  runtimeInputs = [
    myEmacs
    pkgs.emacs-lsp-booster
    pkgs.fd
    pkgs.hurl # required for hurl-mode
    pkgs.multimarkdown
    pkgs.nixd # Nix language server
    pkgs.nodePackages.prettier # formatting files
    pkgs.nixfmt-rfc-style
    pkgs.pandoc
    pkgs.ripgrep
    pkgs.shellcheck # shell script linter
    pkgs.shfmt # formatting bash scripts
    pkgs.silver-searcher
    pkgs.source-code-pro
    pkgs.xclip # X11 clipboard from terminal
  ];
  text = ''
    export LSP_USE_PLISTS="true"
    emacs -q --load ${./emacs.d/init.el} "$@"
  '';
}

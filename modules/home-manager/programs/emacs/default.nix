{
  config,
  lib,
  pkgs,
  unstable-pkgs,
  ...
}:
{
  home.sessionVariables = {
    EMACS_DICT_WORDS = "${pkgs.scowl}/share/dict/wbritish.txt";
    LSP_USE_PLISTS = "true";
  };
  home.packages = [
    pkgs.deadnix
    pkgs.emacs-lsp-booster
    pkgs.fd
    pkgs.hurl # required for hurl-mode
    pkgs.multimarkdown
    pkgs.nixd # Nix language server
    pkgs.prettier # formatting files
    pkgs.nixfmt
    pkgs.pandoc
    pkgs.ripgrep
    pkgs.scowl # word dict for cape
    pkgs.shellcheck # shell script linter
    pkgs.shfmt # formatting bash scripts
    pkgs.silver-searcher
    pkgs.statix
  ]
  ++ lib.optionals (config.programs.emacs.package != pkgs.emacs-nox) [
    pkgs.source-code-pro
    pkgs.xclip # X11 clipboard from terminal
  ];

  systemd.user.services.languagetool = {
    Unit = {
      Description = "Start languagetool";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.jre}/bin/java -cp ${pkgs.languagetool}/share/languagetool-server.jar -Xmx512m org.languagetool.server.HTTPServer";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  systemd.user.services.emacs = {
    Service = {
      Environment = "COLORTERM=truecolor";
    };
  };

  programs.emacs = {
    enable = true;
    extraPackages =
      epkgs:
      [
        epkgs.ace-window
        unstable-pkgs.emacs.pkgs.alabaster-themes
        epkgs.ansible
        epkgs.avy
        epkgs.bind-key
        epkgs.cape
        epkgs.cfrs
        epkgs.citeproc
        epkgs.citar
        epkgs.citar-denote
        epkgs.citar-embark
        (pkgs.callPackage ./packages/claude-code-el {
          inherit (epkgs) inheritenv melpaBuild;
        })
        epkgs.consult
        unstable-pkgs.emacs.pkgs.consult-denote
        epkgs.consult-projectile
        epkgs.corfu
        epkgs.corfu-terminal
        epkgs.dap-mode
        unstable-pkgs.emacs.pkgs.denote
        unstable-pkgs.emacs.pkgs.denote-journal
        unstable-pkgs.emacs.pkgs.denote-org
        epkgs.eglot-booster
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
        epkgs.flymake-ruff
        epkgs.flymake-languagetool
        epkgs.format-all
        epkgs.general
        unstable-pkgs.emacs.pkgs.ghostel
        (pkgs.callPackage ./packages/evil-ghostel {
          inherit (epkgs) melpaBuild evil;
          ghostel = unstable-pkgs.emacs.pkgs.ghostel;
        })
        epkgs.god-mode
        epkgs.haskell-mode
        epkgs.helpful
        epkgs.highlight-indent-guides
        epkgs.ht
        epkgs.htmlize
        (pkgs.callPackage ./packages/hurl-mode {
          inherit (epkgs) melpaBuild;
        })
        epkgs.hydra
        epkgs.hyperbole
        epkgs.jq-mode
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
        epkgs.swiper
        epkgs.treemacs
        epkgs.treemacs-evil
        epkgs.typst-ts-mode
        unstable-pkgs.emacs.pkgs.treesit-grammars.with-all-grammars
        epkgs.ultra-scroll
        epkgs.vertico
        epkgs.vundo
        epkgs.web-mode
        epkgs.wgrep
        epkgs.which-key
        epkgs.yaml-mode
        epkgs.yasnippet-snippets
        pkgs.mu # needed for mailing
      ]
      ++ lib.optionals (config.programs.emacs.package != pkgs.emacs-nox) [
        epkgs.pdf-tools
        epkgs.xclip
      ];
  };
  home.file.".emacs.d/init.el".source = ./emacs.d/init.el;
  home.file.".emacs.d/snippets".source = ./emacs.d/snippets;
  services.emacs = {
    enable = true;
    client.enable = true;
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/text" = "emacsclient.desktop";
      "application/textedit" = "emacsclient.desktop";
      "text/anytext" = "emacsclient.desktop";
      "text/plain" = "emacsclient.desktop";
    };
  };

  programs.bash = {
    enable = true;
  };
}

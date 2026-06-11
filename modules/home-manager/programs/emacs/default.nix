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
      import ./extra-packages.nix {
        inherit
          pkgs
          unstable-pkgs
          epkgs
          lib
          ;
        includeGuiPackages = config.programs.emacs.package != pkgs.emacs-nox;
      };
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

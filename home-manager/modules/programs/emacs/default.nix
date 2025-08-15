{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-emacs;
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system};
in
{
  options = {
    programs.az-emacs.enable = lib.mkEnableOption "Enable emacs.";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      LSP_USE_PLISTS = "true";
    };
    home.packages = [
      pkgs.emacs-lsp-booster
      pkgs.fd
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

    systemd.user.services.languagetool = {
      Unit = {
        Description = "Start languagetool";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.languagetool}/bin/languagetool-http-server";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
      extraPackages = epkgs: [
        epkgs.ag
        epkgs.annotate
        epkgs.ansible
        epkgs.avy
        epkgs.bind-key
        epkgs.cape
        epkgs.cfrs
        epkgs.citeproc
        epkgs.consult
        unstable.emacsPackages.consult-denote
        epkgs.consult-projectile
        epkgs.corfu
        epkgs.corfu-terminal
        epkgs.dap-mode
        unstable.emacsPackages.denote
        unstable.emacsPackages.denote-journal
        unstable.emacsPackages.denote-org
        epkgs.elisp-refs
        unstable.emacsPackages.ellama
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
        epkgs.hydra
        epkgs.hyperbole
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
        epkgs.verb
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
      bashrcExtra = ''
        vterm_printf(){
            if [ -n "$TMUX" ] && ([ "\$\{TERM%%-*}" = "tmux" ] || [ "\$\{TERM%%-*}" = "screen" ] ); then
                # Tell tmux to pass the escape sequences through
                printf "\ePtmux;\e\e]%s\007\e\\" "$1"
            elif [ "\$\{TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$1"
            else
                printf "\e]%s\e\\" "$1"
            fi
        }

        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }
        PS1=$PS1'\[$(vterm_prompt_end)\]'
      '';
    };
  };
}

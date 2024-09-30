{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-emacs;
in
{
  options = {
    programs.az-emacs.enable = lib.mkEnableOption "Enable emacs.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      fd
      multimarkdown
      nixd # Nix language server
      nodePackages.prettier # formatting files
      nodePackages.prettier-plugin-toml
      nixfmt-rfc-style
      pandoc
      ripgrep
      shellcheck # shell script linter
      shfmt # formatting bash scripts
      silver-searcher
      source-code-pro
      xclip # X11 clipboard from terminal
    ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacs29;
      extraConfig = ''
        (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"
                                            "-cp" "${pkgs.languagetool}/share/")
              languagetool-java-bin "${pkgs.jdk17_headless}/bin/java"
              languagetool-console-command "${pkgs.languagetool}/share/languagetool-commandline.jar"
              languagetool-server-command "${pkgs.languagetool}/share/languagetool-server.jar")
      '';
      extraPackages =
        epkgs: with pkgs; [
          languagetool
          epkgs.ag
          epkgs.amx
          epkgs.annotate
          epkgs.ansible
          epkgs.avy
          epkgs.bind-key
          epkgs.cape
          epkgs.cfrs
          epkgs.citeproc
          epkgs.consult
          epkgs.consult-projectile
          epkgs.corfu
          epkgs.corfu-terminal
          epkgs.dap-mode
          unstable.emacsPackages.denote
          epkgs.dired-hide-dotfiles
          epkgs.direnv
          epkgs.elisp-refs
          unstable.emacsPackages.ellama
          epkgs.embark
          epkgs.embark-consult
          epkgs.epl
          epkgs.evil
          epkgs.evil-collection
          epkgs.evil-god-state
          epkgs.evil-surround
          epkgs.f
          epkgs.flymake-flycheck
          epkgs.flymake-shellcheck
          epkgs.format-all
          epkgs.general
          epkgs.god-mode
          unstable.emacsPackages.gptel
          epkgs.haskell-mode
          epkgs.helpful
          epkgs.highlight-indent-guides
          epkgs.ht
          epkgs.htmlize
          epkgs.hydra
          epkgs.hyperbole
          epkgs.know-your-http-well
          epkgs.languagetool
          epkgs.lsp-haskell
          epkgs.lsp-mode
          epkgs.lsp-treemacs
          epkgs.lsp-ui
          epkgs.lv
          epkgs.magit
          epkgs.makey
          epkgs.marginalia
          epkgs.markdown-mode
          epkgs.mu4e
          epkgs.nix-mode
          epkgs.olivetti
          epkgs.orderless
          epkgs.org-modern
          epkgs.org-superstar
          epkgs.ox-pandoc
          epkgs.parsebib
          epkgs.pdf-tools
          epkgs.perspective
          epkgs.pfuture
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
          epkgs.use-package
          epkgs.use-package-ensure-system-package
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
          mu # needed for mailing
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

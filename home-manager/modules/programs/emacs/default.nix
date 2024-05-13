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
      nil # Nix language server
      nodejs_20 # required for copilot
      nodePackages.prettier # formatting files
      nodePackages.prettier-plugin-toml
      unstable.nixfmt-rfc-style
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
      extraConfig = ''
        (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"
                                            "-cp" "${pkgs.languagetool}/share/")
              languagetool-java-bin "${pkgs.jdk17_headless}/bin/java"
              languagetool-console-command "${pkgs.languagetool}/share/languagetool-commandline.jar"
              languagetool-server-command "${pkgs.languagetool}/share/languagetool-server.jar")
      '';
      extraPackages =
        epkgs: with pkgs; [
          # epkgs.journalctl-mode
          # epkgs.magit
          epkgs.ag
          epkgs.amx
          epkgs.annotate
          epkgs.avy
          epkgs.bind-key
          epkgs.cfrs
          epkgs.citeproc
          epkgs.dap-mode
          epkgs.dired-hide-dotfiles
          epkgs.direnv
          epkgs.discover-my-major
          epkgs.elisp-refs
          epkgs.epl
          epkgs.evil
          epkgs.evil-collection
          epkgs.evil-surround
          epkgs.f
          epkgs.flycheck
          epkgs.flymake-shellcheck
          epkgs.format-all
          epkgs.general
          epkgs.helpful
          epkgs.highlight-indent-guides
          epkgs.ht
          epkgs.htmlize
          epkgs.hydra
          epkgs.hyperbole
          epkgs.know-your-http-well
          epkgs.languagetool
          epkgs.lsp-mode
          epkgs.lsp-treemacs
          epkgs.lsp-ui
          epkgs.lv
          epkgs.makey
          epkgs.markdown-mode
          epkgs.move-text
          epkgs.mu4e
          epkgs.nix-mode
          epkgs.olivetti
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
          epkgs.vterm
          epkgs.web-completion-data
          epkgs.web-mode
          epkgs.wgrep
          epkgs.which-key
          epkgs.xclip
          epkgs.yaml-mode
          epkgs.yasnippet-snippets
          languagetool
          mu # needed for mailing
          rufo # formatter for Ruby
          unstable.emacsPackages.cape
          unstable.emacsPackages.consult
          unstable.emacsPackages.corfu
          unstable.emacsPackages.denote
          unstable.emacsPackages.embark
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

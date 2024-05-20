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
          unstable.emacsPackages.ag
          unstable.emacsPackages.amx
          unstable.emacsPackages.annotate
          unstable.emacsPackages.avy
          unstable.emacsPackages.bind-key
          unstable.emacsPackages.cape
          unstable.emacsPackages.cfrs
          unstable.emacsPackages.citeproc
          unstable.emacsPackages.consult
          unstable.emacsPackages.consult-projectile
          unstable.emacsPackages.corfu
          unstable.emacsPackages.corfu-terminal
          unstable.emacsPackages.dap-mode
          unstable.emacsPackages.denote
          unstable.emacsPackages.dired-hide-dotfiles
          unstable.emacsPackages.direnv
          unstable.emacsPackages.elisp-refs
          unstable.emacsPackages.embark
          unstable.emacsPackages.embark-consult
          unstable.emacsPackages.epl
          unstable.emacsPackages.evil
          unstable.emacsPackages.evil-collection
          unstable.emacsPackages.evil-surround
          unstable.emacsPackages.f
          unstable.emacsPackages.flycheck
          unstable.emacsPackages.flymake-shellcheck
          unstable.emacsPackages.format-all
          unstable.emacsPackages.general
          unstable.emacsPackages.helpful
          unstable.emacsPackages.highlight-indent-guides
          unstable.emacsPackages.ht
          unstable.emacsPackages.htmlize
          unstable.emacsPackages.hydra
          unstable.emacsPackages.hyperbole
          unstable.emacsPackages.journalctl-mode
          unstable.emacsPackages.know-your-http-well
          unstable.emacsPackages.languagetool
          unstable.emacsPackages.lsp-mode
          unstable.emacsPackages.lsp-treemacs
          unstable.emacsPackages.lsp-ui
          unstable.emacsPackages.lv
          unstable.emacsPackages.magit
          unstable.emacsPackages.makey
          unstable.emacsPackages.marginalia
          unstable.emacsPackages.markdown-mode
          unstable.emacsPackages.move-text
          unstable.emacsPackages.mu4e
          unstable.emacsPackages.nix-mode
          unstable.emacsPackages.olivetti
          unstable.emacsPackages.orderless
          unstable.emacsPackages.org-modern
          unstable.emacsPackages.org-superstar
          unstable.emacsPackages.ox-pandoc
          unstable.emacsPackages.parsebib
          unstable.emacsPackages.pdf-tools
          unstable.emacsPackages.perspective
          unstable.emacsPackages.pfuture
          unstable.emacsPackages.pkg-info
          unstable.emacsPackages.posframe
          unstable.emacsPackages.powershell
          unstable.emacsPackages.projectile
          unstable.emacsPackages.projectile-ripgrep
          unstable.emacsPackages.python-mode
          unstable.emacsPackages.python-pytest
          unstable.emacsPackages.queue
          unstable.emacsPackages.rainbow-delimiters
          unstable.emacsPackages.ripgrep
          unstable.emacsPackages.smooth-scrolling
          unstable.emacsPackages.solarized-theme
          unstable.emacsPackages.string-inflection
          unstable.emacsPackages.swiper
          unstable.emacsPackages.system-packages
          unstable.emacsPackages.treemacs
          unstable.emacsPackages.treemacs-evil
          unstable.emacsPackages.use-package
          unstable.emacsPackages.use-package-ensure-system-package
          unstable.emacsPackages.verb
          unstable.emacsPackages.vertico
          unstable.emacsPackages.vterm
          unstable.emacsPackages.web-completion-data
          unstable.emacsPackages.web-mode
          unstable.emacsPackages.wgrep
          unstable.emacsPackages.which-key
          unstable.emacsPackages.xclip
          unstable.emacsPackages.yaml-mode
          unstable.emacsPackages.yasnippet-snippets
          unstable.mu # needed for mailing
        ];
    };
    home.file.".emacs.d/init.el".source = ./emacs.d/init.el;
    home.file.".emacs.d/snippets".source = ./emacs.d/snippets;
    services.emacs = {
      enable = true;
      client.enable = true;
      package = pkgs.emacs29;
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

{ pkgs, ... }:
{
  home.packages = with pkgs; [
    multimarkdown
    nodePackages.prettier # formatting files
    pandoc
    python310Packages.python-lsp-server
    ripgrep
    silver-searcher
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
    extraPackages = epkgs: with pkgs;[
      languagetool
      mu # needed for mailing
      rufo # formatter for Ruby
      python310Packages.autopep8
      python310Packages.black
      python310Packages.flake8
      python310Packages.jedi
      python310Packages.pip
      python310Packages.yapf
      epkgs.ace-window
      epkgs.amx
      epkgs.auctex
      epkgs.avy
      epkgs.biblio
      epkgs.biblio-core
      epkgs.bibtex-completion
      epkgs.bind-key
      epkgs.cfrs
      epkgs.citeproc
      epkgs.company
      epkgs.company-auctex
      epkgs.company-restclient
      epkgs.company-web
      epkgs.counsel
      epkgs.deft
      epkgs.dired-hide-dotfiles
      epkgs.direnv
      epkgs.discover-my-major
      epkgs.eglot
      epkgs.elisp-refs
      epkgs.elpy
      epkgs.epl
      epkgs.evil
      epkgs.evil-collection
      epkgs.evil-surround
      epkgs.f
      epkgs.flycheck
      epkgs.format-all
      epkgs.general
      epkgs.helpful
      epkgs.highlight-indent-guides
      epkgs.ht
      epkgs.htmlize
      epkgs.hydra
      epkgs.know-your-http-well
      epkgs.languagetool
      epkgs.lv
      epkgs.magit
      epkgs.makey
      epkgs.markdown-mode
      epkgs.move-text
      epkgs.multi-vterm
      epkgs.nix-mode
      epkgs.org
      epkgs.org-ref
      epkgs.org-superstar
      epkgs.ox-pandoc
      epkgs.parsebib
      epkgs.pdf-tools
      epkgs.perspective
      epkgs.pfuture
      epkgs.pkg-info
      epkgs.posframe
      epkgs.powershell
      epkgs.python-mode
      epkgs.queue
      epkgs.rainbow-delimiters
      epkgs.restclient
      epkgs.smooth-scrolling
      epkgs.solarized-theme
      epkgs.string-inflection
      epkgs.swiper
      epkgs.system-packages
      epkgs.use-package
      epkgs.use-package-ensure-system-package
      epkgs.vterm
      epkgs.web-completion-data
      epkgs.web-mode
      epkgs.wgrep
      epkgs.which-key
      epkgs.xclip
      epkgs.yaml-mode
      epkgs.yasnippet-snippets
      epkgs.zetteldeft
    ];
  };
  home.file.".emacs.d/init.el".source = ./emacs.d/init.el;
  home.file.".emacs.d/snippets".source = ./emacs.d/snippets;
  services.emacs = {
    enable = true;
    client.enable = true;
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
}

{ custom, pkgs, ... }:
{
  home.packages = with pkgs; [
    multimarkdown
    pandoc
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
      mu
      languagetool
      rufo # formatter for Ruby
      python39Packages.autopep8
      python39Packages.black
      python39Packages.flake8
      python39Packages.jedi
      python39Packages.pip
      python39Packages.yapf
      epkgs.ace-window
      epkgs.amx
      epkgs.auctex
      epkgs.avy
      epkgs.biblio
      epkgs.biblio-core
      epkgs.bibtex-completion
      epkgs.bind-key
      epkgs.bug-hunter
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
      epkgs.evil-surround
      epkgs.eyebrowse
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
      epkgs.nix-mode
      epkgs.org
      epkgs.org-ref
      epkgs.org-superstar
      epkgs.ox-pandoc
      epkgs.parsebib
      epkgs.pdf-tools
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
      epkgs.treemacs
      epkgs.treemacs-evil
      epkgs.use-package
      epkgs.use-package-ensure-system-package
      epkgs.web-completion-data
      epkgs.web-mode
      epkgs.wgrep
      epkgs.which-key
      epkgs.yaml-mode
      epkgs.yasnippet-snippets
    ];
  };
  home.file.".emacs.d/init.el".source = ./emacs.d/init.el;
  home.file.".emacs.d/loader.org".source = ./emacs.d/loader.org;
  home.file.".emacs.d/snippets".source = ./emacs.d/snippets;
  home.file.".Eshell/alias".source = ./Eshell/alias;
  services.emacs = {
    enable = true;
    client.enable = true;
  };
}

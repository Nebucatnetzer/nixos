{ config, lib, pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  programs = {
    home-manager.enable = true;
    fzf = {
      enable = true;
      enableBashIntegration = true;
    };
    starship = {
      settings = {
        add_newline = false;
        format = lib.concatStrings [
          "$username"
          "$hostname"
          "$directory"
          "$nix_shell"
          "$python"
          "$git_branch"
          "$git_status"
          "$character"
        ];
        nix_shell = {
          format = "\\[X\\] ";
        };
        python = {
          format = "[$\{symbol\}($virtualenv) ]($style)";
        };
      };
      enable = true;
    };
    vim = {
      enable = true;
      settings = {
        expandtab = true;
        tabstop = 4;
        shiftwidth = 4;
        number = true;
        relativenumber = true;
      };
      extraConfig = ''
        autocmd BufWritePre * :%s/\s\+$//e
        if has("autocmd")
          au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
        endif

        set cursorline " highlight the current line
        set wildmenu " visual autocomplete for command menu
        set background=dark " enables the dark background in themes
        set showmatch " highlight matching [{()}]

        "Shows a red line with a width of 80 characters
        set colorcolumn=80
        "Wrap text after 80 characters
        set textwidth=80
        "Autowrap text with textwidh (t) and only insert a comment leader when pressing
        "o or O (o)
        set formatoptions=to
        set encoding=utf-8
        highlight BadWhitespace ctermbg=red guibg=darkred
        au BufRead,BufNewFile * match BadWhitespace /\s\+$/
      '';
    };
  };

  home = {
    homeDirectory = "/home/${config.home.username}";
    stateVersion = "22.11";
    sessionVariables = {
      EDITOR = "vim";
      HIGHLIGHT_STYLE = "solarized-light";
      HISTTIMEFORMAT = "%F %T ";
      NIXPKGS_ALLOW_UNFREE = "1";
    };

    shellAliases = {
      format-modules = "nixpkgs-fmt **/*.nix";
      nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
      rebuild = ''
        nixos-rebuild -j auto switch --use-remote-sudo
      '';
      find-garbage = "ls -l /nix/var/nix/gcroots/auto/ | sort";
      vm = "vim";
      less = "less -FiRX";
      ls = "ls -lhF";
      btm = "btm --color default-light";
    };
    packages = with pkgs; [
      bottom
      git
      highlight
      htop
      killall
      ncdu
      nixpkgs-fmt
      nmon
      tree
      unzip
      wget
    ];
  };
}

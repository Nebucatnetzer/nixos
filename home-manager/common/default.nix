{ custom }: { pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  programs.home-manager.enable = true;
  home = {
    username = custom.username;
    homeDirectory = "/home/${custom.username}";
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

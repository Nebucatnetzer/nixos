{ custom }: { pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = custom.username;
  home.homeDirectory = "/home/${custom.username}";
  home.stateVersion = custom.version;
  programs.home-manager.enable = true;
  home.shellAliases = {
    format-modules = "nixpkgs-fmt **/*.nix";
    nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
    rebuild = ''
      nixos-rebuild -j auto switch --use-remote-sudo
    '';
    find-garbage = "ls -l /nix/var/nix/gcroots/auto/ | sort";
    vm = "vim";
    less = "less -FiRX";
    ls = "ls -lhF";
  };
  home.packages = with pkgs; [
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
}

{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    git
    highlight
    htop
    killall
    ncdu
    nixpkgs-fmt
    ranger
    tmux
    tree
    unzip
    vim
    wget
  ];
  environment.shellAliases = {
    format-modules = "nixpkgs-fmt **/*.nix";
    nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
    rebuild = "sudo nixos-rebuild -j auto switch";
    find-garbage = "ls -l /nix/var/nix/gcroots/auto/ | sort";
    vm = "vim";
  };
}


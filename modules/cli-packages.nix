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
    format = "nixpkgs-fmt **/*.nix";
  };
}


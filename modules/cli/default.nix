{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    git
    highlight
    htop
    killall
    ncdu
    nixpkgs-fmt
    nmon
    tree
    unzip
    vim
    wget
  ];
}


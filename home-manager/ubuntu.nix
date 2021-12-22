{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./software/git
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/git_repos/nixos/home-manager/configs/bash/bashrc
    '';
  };
  targets.genericLinux.enable = true;
}

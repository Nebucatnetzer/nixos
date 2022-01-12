{ ... }:
{
  imports = [
    ./common
    ./software/git
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/.nixos/home-manager/configs/bash/bashrc
    '';
  };
  targets.genericLinux.enable = true;
}

{ pkgs, ... }:
let
  username = import ../username.nix;
in
{
  imports = [
    ./common
    ./software/git
  ];

  home.packages = with pkgs; [
    vagrant
  ];

  programs.git.userEmail = "zweili@contria.com";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/.nixos/home-manager/configs/bash/work_wsl_bashrc
      . /home/${username}/.nix-profile/etc/profile.d/nix.sh
    '';
    sessionVariables = {
      VAGRANT_WSL_ENABLE_WINDOWS_ACCESS = 1;
      PATH = "$PATH:/mnt/c/Program Files/Oracle/VirtualBox";
    };
  };
}

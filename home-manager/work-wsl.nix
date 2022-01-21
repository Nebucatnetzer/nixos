{ pkgs, ... }:
let
  username = import ../username.nix;
in
{
  imports = [
    ./common
    ./software/ansible-2-9
    ./software/git
    ./software/vagrant-wsl
    ./software/vim
  ];

  programs.git.userEmail = "zweili@contria.com";
  home.packages = with pkgs; [
    xonsh
  ];

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/.nixos/home-manager/configs/bash/work_wsl_bashrc
      . /home/${username}/.nix-profile/etc/profile.d/nix.sh
    '';
  };
}

{ inputs, pkgs, ... }:
{
  imports = [
    ./common
    ./software/ansible-2-9
    ./software/git
    ./software/vagrant-wsl
    ./software/vim
  ];

  programs.git.userEmail = "zweili@contria.com";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/.nixos/home-manager/configs/bash/work_wsl_bashrc
      . /home/${inputs.custom.username}/.nix-profile/etc/profile.d/nix.sh
    '';
  };
}

{ inputs, custom, pkgs, ... }:
{
  imports = [
    ./common
    ./software/ansible
    ./software/emacs
    ./software/fzf
    ./software/git
    ./software/starship
    ./software/vagrant-wsl
    ./software/vim
  ];

  programs.git.userEmail = "zweili@contria.com";
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    nixpkgs-fmt
    mosh
  ];

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/.nixos/home-manager/configs/bash/work_wsl_bashrc
      . /home/${custom.username}/.nix-profile/etc/profile.d/nix.sh
    '';
  };
}

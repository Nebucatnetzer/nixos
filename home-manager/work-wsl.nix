{ inputs, custom }: { pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/common"
    "${inputs.self}/home-manager/software/ansible"
    "${inputs.self}/home-manager/software/emacs"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/starship"
    "${inputs.self}/home-manager/software/vagrant-wsl"
    "${inputs.self}/home-manager/software/vim"
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

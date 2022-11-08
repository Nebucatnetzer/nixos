{ custom, pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/home-manager/common" { inherit custom; })
    "${custom.inputs.self}/home-manager/software/ansible"
    "${custom.inputs.self}/home-manager/software/emacs"
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/starship"
    "${custom.inputs.self}/home-manager/software/vagrant-wsl"
    "${custom.inputs.self}/home-manager/software/vim"
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

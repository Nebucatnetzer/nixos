{ inputs, nixosConfig, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/ssh"
    "${inputs.self}/home-manager/software/starship"
    "${inputs.self}/home-manager/software/vim"
  ];

  home.username = nixosConfig.az-username;
  home.packages = with pkgs; [
    docker-compose
    exercism
    nodePackages.prettier # formatting files
    rclone
    xclip
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs = {
    az-emacs.enable = true;
    bash = {
      enable = true;
    };
  };

}

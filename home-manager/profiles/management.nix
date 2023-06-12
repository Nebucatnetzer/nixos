{ inputs, nixosConfig, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
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

  programs = {
    az-emacs.enable = true;
    az-git.enable = true;
    az-ssh.enable = true;
    bash = {
      enable = true;
    };
  };

}

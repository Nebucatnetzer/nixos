{ inputs, nixosConfig, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];

  home.username = nixosConfig.az-username;
  home.packages = with pkgs; [
    docker-compose
    exercism
    nodePackages.prettier # formatting files
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

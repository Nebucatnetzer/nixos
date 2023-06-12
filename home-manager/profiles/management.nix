{ inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];

  home.packages = with pkgs; [
    docker-compose
    exercism
    nodePackages.prettier # formatting files
    xclip
  ];

  programs = {
    az-emacs.enable = true;
    az-ssh.enable = true;
  };

}

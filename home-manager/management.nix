{ custom, system }: { config, nixosConfig, pkgs, ... }:
{
  imports = [
    "${custom.inputs.self}/home-manager/common"
    "${custom.inputs.self}/home-manager/software/emacs"
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/ssh"
    "${custom.inputs.self}/home-manager/software/starship"
    "${custom.inputs.self}/home-manager/software/vim"
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

  programs.bash = {
    enable = true;
  };

}

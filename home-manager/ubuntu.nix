{ inputs, nixosConfig, ... }:
{
  imports = [
    "${inputs.self}/home-manager/common"
    "${inputs.self}/home-manager/software/git"
  ];

  home.username = nixosConfig.az-username;

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
  targets.genericLinux.enable = true;
}

{ inputs, nixosConfig, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];

  home.username = nixosConfig.az-username;

  programs = {
    az-git.enable = true;
    bash = {
      enable = true;
    };
  };
  targets.genericLinux.enable = true;
}

{ inputs, nixosConfig, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
    "${inputs.self}/home-manager/software/vim"
    "${inputs.self}/home-manager/software/starship"
  ];

  home.username = nixosConfig.az-username;

  programs = {
    az-git.enable = true;
    bash = {
      enable = true;
    };
  };
}

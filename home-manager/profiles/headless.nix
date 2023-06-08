{ inputs, nixosConfig, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/vim"
    "${inputs.self}/home-manager/software/starship"
  ];

  home.username = nixosConfig.az-username;
  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
}

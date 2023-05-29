{ custom, system }: { config, nixosConfig, ... }:
{
  imports = [
    "${custom.inputs.self}/home-manager/common"
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/vim"
    "${custom.inputs.self}/home-manager/software/starship"
  ];

  home.username = nixosConfig.az-username;
  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
}

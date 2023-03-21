{ custom, system }: { ... }:
{
  imports = [
    (import "${custom.inputs.self}/home-manager/common" { inherit custom; })
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/vim"
    "${custom.inputs.self}/home-manager/software/starship"
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
}

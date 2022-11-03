{ custom, inputs }: { ... }:
{
  imports = [
    (import "${inputs.self}/home-manager/common" { inherit custom inputs; })
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/vim"
    "${inputs.self}/home-manager/software/starship"
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
}

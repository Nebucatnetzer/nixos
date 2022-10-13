{ inputs, custom, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/home-manager/common" { inherit custom inputs; })
    "${inputs.self}/home-manager/software/emacs"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/ssh"
    "${inputs.self}/home-manager/software/starship"
    "${inputs.self}/home-manager/software/vim"
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

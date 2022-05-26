{ inputs, custom, pkgs, ... }:
{
  imports = [
    (import ./common { inherit custom inputs; })
    ./software/fzf
    ./software/git
    ./software/vim
    ./software/starship
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

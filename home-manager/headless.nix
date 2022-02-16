{ inputs, pkgs, ... }:
{
  imports = [
    (import ./common { inherit inputs; })
    ./software/git
    ./software/vim
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

{ pkgs, username, ... }:
{
  imports = [
    (import ./common { inherit username; })
    ./software/git
    ./software/vim
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

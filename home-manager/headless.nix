{ self, pkgs, ... }:
{
  imports = [
    (import ./common { inherit self; })
    ./software/git
    ./software/vim
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

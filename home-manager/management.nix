{ inputs, custom, pkgs, ... }:
{
  imports = [
    (import ./common { inherit custom inputs; })
    ./software/emacs
    ./software/email
    ./software/git
    ./software/vim
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

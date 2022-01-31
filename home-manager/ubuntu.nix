{ ... }:
{
  imports = [
    ./common
    ./software/git
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
  targets.genericLinux.enable = true;
}

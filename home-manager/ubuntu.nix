{config, pkgs, ...}:
{
  imports = [
    ./common.nix
    ./common/git/git.nix
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  #programs.bash.enable = true;
  targets.genericLinux.enable = true;
}

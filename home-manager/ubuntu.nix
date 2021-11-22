{config, pkgs, ...}:
{
  imports = [
    ./common.nix
    ./common/git/git.nix
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/git_repos/nixos/home-manager/personal_config/bashrc
    '';
  };
  targets.genericLinux.enable = true;
}

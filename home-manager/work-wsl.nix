{config, pkgs, ...}:
{
  imports = [
    ./common.nix
    ./common/git/git.nix
  ];
  programs.git.userEmail = "zweili@contria.com";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/git_repos/nixos/home-manager/work_config/bashrc
      . /home/andreas/.nix-profile/etc/profile.d/nix.sh
    '';
  };
}

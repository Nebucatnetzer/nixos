{ pkgs, ... }:
let
  username = import ../username.nix;
in
{
  imports = [
    ./common
    ./software/git
  ];
  programs.git.userEmail = "zweili@contria.com";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/git_repos/nixos/home-manager/work_config/bashrc
      . /home/${username}/.nix-profile/etc/profile.d/nix.sh
    '';
  };
}

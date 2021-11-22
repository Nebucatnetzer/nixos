{config, pkgs, ...}:
{
  imports = [
    ./common.nix
    ./common/git/git.nix
  ];
  programs.git.userEmail = "zweili@contria.com";

  home.shellAliases = {
     management-server = "mosh --ssh=\"ssh -p 22\" localadmin@10.40.0.53 tmux a";
  };

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ~/git_repos/nixos/home-manager/work_config/bashrc
    '';
  };
}

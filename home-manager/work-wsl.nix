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
  # raw config files
  home.file.".bashrc".source = ./work_config/bashrc;
}

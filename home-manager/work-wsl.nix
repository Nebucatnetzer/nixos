{config, pkgs, ...}:
{ 
  imports = [
    ./common.nix
  ];
  programs.git = {
    enable = true;
    userName = "Andreas Zweili";
    userEmail = "zweili@contria.com";
    delta = {
      enable = true;
      options = {
        navigate = true;
        line-numbers = true;
        syntax-theme = "GitHub";
      };
    };
  };
  home.shellAliases = {
     management-server = "mosh --ssh=\"ssh -p 22\" localadmin@10.40.0.53 tmux a";
  };
  # raw config files
  home.file.".bashrc".source = ./work_config/bashrc;
}

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
  # raw config files
  home.file.".bashrc".source = ./work_config/bashrc;
}

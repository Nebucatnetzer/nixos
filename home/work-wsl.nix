{config, pkgs, ...}:
{ 
  imports = [
    ./common.nix
  ];
  programs.git = {
    enable = true;
    userName = "Andreas Zweili";
    userEmail = "zweili@contria.com";
  };
  # raw config files
  home.file.".bashrc".source = ./work_config/bashrc;
}

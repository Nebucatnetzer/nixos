{config, pkgs, ...}:
{ 
  imports = [
    ./common.nix
  ];
  programs.git = {
    enable = true;
    userName = "Andreas Zweili";
    userEmail = "andreas@zweili.ch";
  };
  # raw config files
  home.file.".bashrc".source = ./work_config/bashrc;
}

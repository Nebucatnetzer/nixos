{config, pkgs, ...}:
{ 
  imports = [
    ./common.nix
  ];
  home.packages = with pkgs; [
  ];
  programs.git = {
    enable = true;
    userName = "Andreas Zweili";
    userEmail = "andreas@zweili.ch";
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
  home.file.".bashrc".source = ./personal_config/bashrc;
}

{config, pkgs, ...}:
{ 
  imports = [
    ./common.nix
  ];
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    keeweb
    nextcloud-client
    signal-desktop
    tdesktop
    vscode
    youtube-dl
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
  home.file.".config/qtile".source = ./desktop/qtile;
}

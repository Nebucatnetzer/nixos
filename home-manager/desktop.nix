{ config, pkgs, ... }:
{
  imports = [
    ./common.nix
    ./common/git/git.nix
  ];
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    keeweb
    signal-desktop
    tdesktop
    vscode
    youtube-dl
  ];
  programs.git.userEmail = "andreas@zweili.ch";

  # raw config files
  home.file.".config/qtile".source = ./desktop/qtile;
  home.file.".config/terminator".source = ./desktop/terminator;

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  services.network-manager-applet.enable = true;

}

{ config, pkgs, ... }:
{
  imports = [
    ./common.nix
    ./common/git/git.nix
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
  programs.git.userEmail = "andreas@zweili.ch";

  # raw config files
  home.file.".config/qtile".source = ./desktop/qtile;
  home.file.".xprofile".source = ./desktop/xprofile;
  home.file.".config/terminator".source = ./desktop/terminator;

}

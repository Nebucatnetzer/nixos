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
  # home.file.".xprofile".source = ./desktop/xprofile;
  home.file.".config/terminator".source = ./desktop/terminator;

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  services.network-manager-applet.enable = true;

  systemd.user.services.telegram-desktop = {
    description = "Telegram Desktop";
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.tdesktop}/bin/telegram-desktop";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };
  #
  # systemd.user.services.signal-desktop = {
  # description = "Signal Desktop";
  # partOf = [ "graphical-session.target" ];
  # wantedBy = [ "graphical-session.target" ];
  # serviceConfig = {
  # Type = "simple";
  # ExecStart = "${pkgs.signal-desktop}/bin/signal-desktop";
  # ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
  # KillMode = "process";
  # Restart = "on-failure";
  # };
  # };
  #
  # systemd.user.services.keeweb = {
  # description = "Keeweb";
  # partOf = [ "graphical-session.target" ];
  # wantedBy = [ "graphical-session.target" ];
  # serviceConfig = {
  # Type = "simple";
  # ExecStart = "${pkgs.keeweb}/bin/keeweb";
  # ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
  # KillMode = "process";
  # Restart = "on-failure";
  # };
  # };

}

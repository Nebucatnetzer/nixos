# needs to be imported into the main nix config
{ config, pkgs, ... }:
{
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
}

# needs to be imported into the main nix config
{ config, pkgs, ... }:
{
  systemd.user.services.signal-desktop = {
    description = "Signal Desktop";
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.signal-desktop}/bin/signal-desktop";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };
}

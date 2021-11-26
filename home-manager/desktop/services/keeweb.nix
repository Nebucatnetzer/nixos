# needs to be imported into the main nix config
{ config, pkgs, ... }:
{
  systemd.user.services.keeweb = {
    description = "Keeweb";
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.keeweb}/bin/keeweb";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };
}

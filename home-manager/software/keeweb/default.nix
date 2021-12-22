{ pkgs, ... }:
{
  home.packages = with pkgs; [
    keeweb
  ];

  systemd.user.services.keeweb = {
    Unit = {
      description = "KeeWeb";
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.keeweb}/bin/keeweb";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };
}

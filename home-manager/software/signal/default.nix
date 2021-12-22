{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.signal-desktop
  ];

  systemd.user.services.signal-desktop = {
    Unit = {
      description = "Signal Desktop";
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.unstable.signal-desktop}/bin/signal-desktop";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };
}

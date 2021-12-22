{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.tdesktop
  ];

  systemd.user.services.telegram-desktop = {
    Unit = {
      description = "Telegram Desktop";
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.unstable.tdesktop}/bin/telegram-desktop";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };
}

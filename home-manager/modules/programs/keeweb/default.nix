{ config, lib, pkgs, ... }:
let cfg = config.programs.az-keeweb;
in {
  options = {
    programs.az-keeweb.enable = lib.mkEnableOption "Enable keeweb.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ keeweb ];
    systemd.user.services.keeweb = {
      Unit = {
        Description = "Start keeweb";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Environment = "QT_SCALE_FACTOR=1.25";
        ExecStart = "${pkgs.keeweb}/bin/keeweb --no-sandbox";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}

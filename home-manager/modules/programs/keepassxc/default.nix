{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-keepassxc;
in
{
  options = {
    programs.az-keepassxc.enable = lib.mkEnableOption "Enable keepassxc.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      keepassxc
    ];
    systemd.user.services.keepassxc = {
      Unit = {
        Description = "Start keepassxc";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Environment = "QT_SCALE_FACTOR=1.25";
        ExecStart = "${pkgs.keepassxc}/bin/keepassxc";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}

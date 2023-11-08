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
        ExecStart = "${pkgs.keepassxc}/bin/keepassxc";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-logs-share;
in
{
  options = {
    services.az-logs-share.enable = lib.mkEnableOption "Enable mount share";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.lnav ];
    fileSystems."/mnt/server_logs" = {
      device = "10.7.89.108:logs";
      fsType = "nfs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=300"
        "noatime"
        "nfsvers=4.0"
        "ro"
      ];
    };
  };
}

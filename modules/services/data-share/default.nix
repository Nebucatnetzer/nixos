{ config, lib, ... }:
let
  cfg = config.services.az-data-share;
in
{
  options = {
    services.az-data-share.enable = lib.mkEnableOption "Enable the server data FNS share";
  };
  config = lib.mkIf cfg.enable {
    fileSystems."/mnt/data" = {
      device = "10.7.89.108:server_data";
      fsType = "nfs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=300"
        "noatime"
        "nfsvers=4.0"
        "noresvport" # don't bind to privileged port
      ];
    };
  };
}

{ config, lib, ... }:
let
  cfg = config.services.az-media-share;
in
{
  options = {
    services.az-media-share.enable = lib.mkEnableOption "Mount the NFS share for with my media.";
    services.az-media-share.hard = lib.mkEnableOption "Enable the hard option for NFS.";
  };

  config = lib.mkIf cfg.enable {
    fileSystems."/mnt/media" = {
      device = "10.7.89.108:media";
      fsType = "nfs";
      options = lib.mkMerge [
        [
          "noatime"
          "nfsvers=4.0"
        ]
        (lib.mkIf cfg.hard [
          "hard"
        ])
        (lib.mkIf (!cfg.hard) [
          "noauto"
          "_netdev"
          "x-systemd.idle-timeout=300"
          "x-systemd.automount"
          "x-systemd.mount-timeout=5"
        ])
      ];
    };
  };
}

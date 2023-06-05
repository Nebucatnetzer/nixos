{ config, lib, ... }:
let
  cfg = config.services.az-media-share;
in
{
  options = {
    services.az-media-share.enable = lib.mkEnableOption "Mount the NFS share for with my media.";
  };

  config = lib.mkIf cfg.enable {
    fileSystems."/mnt/media" = {
      device = "10.7.89.108:media";
      fsType = "nfs";
      options = [ "noatime" "hard" "nfsvers=4.0" ];
    };
  };
}

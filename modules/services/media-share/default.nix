{
  hard ? false,
}:
{ lib, ... }:
{
  fileSystems."/mnt/fileserver/media" = {
    device = "10.7.89.108:media";
    fsType = "nfs";
    options = lib.mkMerge [
      [
        "noatime"
        "nfsvers=4.0"
      ]
      (lib.mkIf hard [
        "hard"
      ])
      (lib.mkIf (!hard) [
        "noauto"
        "_netdev"
        "x-systemd.idle-timeout=300"
        "x-systemd.automount"
        "x-systemd.mount-timeout=5"
      ])
    ];
  };
}

{ ... }:
{
  fileSystems."/mnt/fileserver/media" = {
    device = "10.7.89.108:media";
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
}

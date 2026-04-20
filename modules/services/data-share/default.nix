{ config, ... }:
{
  fileSystems."/mnt/fileserver/data" = {
    device = "${config.az-hosts.fileserver.physicalIp}:server_data";
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

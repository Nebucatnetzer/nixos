# Soft automount of gwyn's archive, at the same path the disk has on gwyn.
{
  path,
}:
{ config, ... }:
{
  # noauto plus automount and a short mount timeout.
  fileSystems."${path}" = {
    device = "${config.az-hosts.gwyn.wgIp}:${path}";
    fsType = "nfs";
    options = [
      "_netdev"
      "nfsvers=4.2"
      "noatime"
      "noauto"
      "x-systemd.automount"
      "x-systemd.idle-timeout=300"
      "x-systemd.mount-timeout=5"
    ];
  };
}

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
      # timeo is deciseconds and doubles per retransmission over TCP, so this budget is
      # 10+20+40+80, about 150s. Long enough to sit through a gwyn reboot, short enough
      # that a dead gwyn degrades instead of blocking forever the way hard would.
      "retrans=4"
      # softerr rather than soft: both give up, but softerr returns ETIMEDOUT instead of
      # EIO, so "the server went away" stays distinguishable from "the data is bad" on a
      # share holding the only copy of the FLAC and RAW masters. Implies softreval, which
      # is what lets path walking fall back to cached attributes while gwyn is gone.
      "softerr"
      "timeo=100"
      "x-systemd.automount"
      "x-systemd.idle-timeout=300"
      "x-systemd.mount-timeout=5"
    ];
  };
}

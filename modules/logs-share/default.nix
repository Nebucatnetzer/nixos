{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    unstable.lnav
  ];
  fileSystems."/mnt/server_logs" = {
    device = "10.7.89.108:logs";
    fsType = "nfs";
    options = [ "x-systemd.automount" "noauto" "x-systemd.idle-timeout=300" "noatime" "nfsvers=4.0" ];
  };
}

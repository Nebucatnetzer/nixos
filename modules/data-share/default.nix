{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    lnav
  ];
  fileSystems."/mnt/data" = {
    device = "10.7.89.108:server_data";
    fsType = "nfs";
    options = [ "x-systemd.automount" "noauto" "x-systemd.idle-timeout=300" "noatime" "nfsvers=4.0" ];
  };
}

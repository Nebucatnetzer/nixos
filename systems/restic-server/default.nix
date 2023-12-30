{ hostname }: { inputs, pkgs, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.30";
    };
  };
  fileSystems."/var/lib/restic-server" = {
    device = "/dev/disk/by-label/backups";
    fsType = "ext4";
    options = [ "hard" ];
  };
  services = {
    az-restic-server.enable = true;
  };
}

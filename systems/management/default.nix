{ hostname }: { inputs, ... }:
{
  fileSystems = {
    "/mnt/external" = {
      device = "/dev/disk/by-uuid/F73C-AA4F";
      fsType = "exfat";
      options = [ "x-systemd.automount" "noauto" "noatime" "uid=1000" "gid=100" ];
    };
  };

  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.150";
    };
  };
  # Features
  services = {
    az-data-share.enable = true;
    az-docker.enable = true;
    az-logs-share.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    };
  };
  # Enable dictionaries
  programs = {
    az-hunspell.enable = true;
    az-nix-direnv.enable = true;
    az-restic-management.enable = true;
  };

}

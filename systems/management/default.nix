{ hostname }: { inputs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit hostname;
    })
    (import "${inputs.self}/modules/restic-client-server" {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    })
  ];
  fileSystems = {
    "/mnt/external" = {
      device = "/dev/disk/by-uuid/F73C-AA4F";
      fsType = "exfat";
      options = [ "x-systemd.automount" "noauto" "noatime" "uid=1000" "gid=100" ];
    };
  };

  # Features
  services = {
    az-data-share.enable = true;
    az-docker.enable = true;
    az-logs-share.enable = true;
    az-rclone-webdav.enable = true;
  };
  # Enable dictionaries
  programs = {
    az-hunspell.enable = true;
    az-nix-direnv.enable = true;
    az-tmux.enable = true;
  };

}

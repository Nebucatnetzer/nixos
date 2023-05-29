{ hostname }: { inputs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit hostname;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/logs-share"
    "${inputs.self}/modules/nix-direnv"
    "${inputs.self}/modules/rclone-webdav"
    (import "${inputs.self}/modules/restic-client-server" {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    })
    "${inputs.self}/modules/tmux"
  ];
  fileSystems = {
    "/mnt/external" = {
      device = "/dev/disk/by-uuid/F73C-AA4F";
      fsType = "exfat";
      options = [ "x-systemd.automount" "noauto" "noatime" "uid=1000" "gid=100" ];
    };
  };
  services.az-data-share.enable = true;
}

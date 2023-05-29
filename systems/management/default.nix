{ custom, hostname }: { ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit custom hostname;
    })
    "${custom.inputs.self}/modules/docker"
    "${custom.inputs.self}/modules/data-share"
    "${custom.inputs.self}/modules/logs-share"
    "${custom.inputs.self}/modules/nix-direnv"
    (import "${custom.inputs.self}/modules/rclone-webdav" { inherit custom; })
    (import "${custom.inputs.self}/modules/restic-client-server" {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
      inherit custom;
    })
    "${custom.inputs.self}/modules/tmux"
  ];
  fileSystems = {
    "/mnt/external" = {
      device = "/dev/disk/by-uuid/F73C-AA4F";
      fsType = "exfat";
      options = [ "x-systemd.automount" "noauto" "noatime" "uid=1000" "gid=100" ];
    };
  };
}

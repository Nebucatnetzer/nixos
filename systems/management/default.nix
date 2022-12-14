{ custom, hostname }: { ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/data-share"
    "${custom.inputs.self}/modules/logs-share"
    (import "${custom.inputs.self}/modules/nix-direnv" { inherit custom; })
    (import "${custom.inputs.self}/modules/restic-server-client" {
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

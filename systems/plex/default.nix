{ custom, hostname, inputs }: { pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.112";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      path = "/var/lib/plex";
      tag = "plex";
      time = "03:30"; inherit custom hostname inputs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/media-share"
    "${inputs.self}/modules/plex"
  ];
}

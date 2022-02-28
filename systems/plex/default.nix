{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "plex";
      ip = "10.7.89.112";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "03:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/media-share"
    "${inputs.self}/modules/plex"
  ];
}

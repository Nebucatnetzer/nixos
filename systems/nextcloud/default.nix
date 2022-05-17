{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "nextcloud";
      ip = "10.7.89.103";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "04:00"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
  ];
}

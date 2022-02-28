{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "pihole";
      ip = "10.7.89.2";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "05:00"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/pihole"
    "${inputs.self}/modules/unbound"
  ];
}

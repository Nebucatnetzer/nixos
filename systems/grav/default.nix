{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "grav";
      ip = "10.7.89.102";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "22:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
  ];
}

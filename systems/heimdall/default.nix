{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "heimdall";
      ip = "10.7.89.121";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "22:00"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
  ];
}

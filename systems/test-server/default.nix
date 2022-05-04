{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "test-server";
      ip = "10.7.89.142";
      inherit inputs;
    })
    "${inputs.self}/modules/docker"
  ];
}

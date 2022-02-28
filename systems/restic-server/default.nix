{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "restic-server";
      ip = "10.7.89.30";
      inherit inputs;
    })
    "${inputs.self}/modules/restic-server"
  ];
}

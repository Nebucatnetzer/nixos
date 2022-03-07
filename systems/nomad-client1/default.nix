{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "nomad-agent1";
      ip = "10.7.89.141";
      inherit inputs;
    })
    "${inputs.self}/modules/data-share"
    "${inputs.self}/modules/nomad-client"
  ];
}

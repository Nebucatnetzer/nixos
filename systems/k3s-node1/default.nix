{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "k3s-node1";
      ip = "10.7.89.131";
      inherit inputs;
    })
    "${inputs.self}/modules/data-share"
    "${inputs.self}/modules/k3s-node"
  ];
}

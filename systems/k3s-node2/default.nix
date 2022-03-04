{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "k3s-node2";
      ip = "10.7.89.132";
      inherit inputs;
    })
    "${inputs.self}/modules/k3s-node"
  ];
}

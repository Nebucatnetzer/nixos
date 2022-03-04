{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "k3s-master1";
      ip = "10.7.89.130";
      inherit inputs;
    })
    "${inputs.self}/modules/k3s-master"
  ];
}

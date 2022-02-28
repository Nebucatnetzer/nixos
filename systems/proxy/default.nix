{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "proxy";
      ip = "10.7.89.100";
      inherit inputs;
    })
    "${inputs.self}/modules/haproxy"
  ];
}

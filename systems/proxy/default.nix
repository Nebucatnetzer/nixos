{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "proxy";
      ip = "10.7.89.100";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "21:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/haproxy"
  ];
}

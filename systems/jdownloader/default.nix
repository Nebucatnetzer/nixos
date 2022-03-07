{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "jdownloader";
      ip = "10.7.89.110";
      inherit inputs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/jdownloader"
  ];
}

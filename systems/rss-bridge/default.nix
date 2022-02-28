{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "rss-bridge";
      ip = "10.7.89.111";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "23:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
  ];
}

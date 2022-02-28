{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "ttrss";
      ip = "10.7.89.115";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "23:00"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
  ];
}

{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "git";
      ip = "10.7.89.109";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "03:00"; inherit config custom inputs pkgs;
    })
    (import "${inputs.self}/modules/nginx-acme" {
      domain = "git.2li.ch"; inherit inputs;
    })
    "${inputs.self}/modules/docker"
  ];
}

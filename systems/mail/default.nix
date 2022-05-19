{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "mail";
      ip = "10.7.89.123";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "04:30"; inherit config custom inputs pkgs;
    })
    (import "${inputs.self}/modules/nginx-acme" {
      domain = "mail.zweili.org"; inherit inputs;
    })
    "${inputs.self}/modules/docker"
  ];
}


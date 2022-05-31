{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "test-server";
      ip = "10.7.89.142";
      inherit inputs;
    })
    (import "${inputs.self}/modules/nginx-acme" {
      domain = "test.2li.ch";
      inherit inputs;
    })
    "${inputs.self}/modules/docker"
  ];
}

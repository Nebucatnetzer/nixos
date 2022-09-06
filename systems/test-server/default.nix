{ custom, hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.142";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/nginx-acme" {
      domain = "test.2li.ch";
      inherit inputs;
    })
    "${inputs.self}/modules/docker"
  ];
}

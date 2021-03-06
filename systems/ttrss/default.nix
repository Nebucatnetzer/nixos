{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "ttrss";
      ip = "10.7.89.115";
      inherit inputs;
    })
    (import "${inputs.self}/modules/nginx-acme" {
      domain = "ttrss.2li.ch"; inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-mysql-client" {
      time = "23:00"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/mariadb"
  ];
}

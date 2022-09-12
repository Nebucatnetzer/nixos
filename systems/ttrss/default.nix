{ custom, hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.115";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      domain = "ttrss.2li.ch"; inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-mysql-client" {
      time = "23:00"; inherit custom hostname inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/mariadb"
  ];
}

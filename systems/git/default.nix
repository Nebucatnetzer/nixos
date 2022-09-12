{ custom, hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.109";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-mysql-client" {
      time = "03:00"; inherit custom hostname inputs pkgs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      domain = "git.2li.ch"; inherit inputs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/mariadb"
  ];
}

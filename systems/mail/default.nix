{ custom, hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.123";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "04:30"; inherit custom hostname inputs pkgs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      domain = "mail.zweili.org"; inherit inputs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/mariadb"
  ];
}


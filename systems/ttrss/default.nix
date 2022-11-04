{ custom, hostname, inputs }: { pkgs, ... }:
let
  domain = "ttrss.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.115";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      inherit domain inputs;
    })
    (import "${inputs.self}/modules/restic-server-mysql-client" {
      path = "/var/lib/ttrss";
      tag = "ttrss";
      time = "23:00"; inherit inputs;
    })
    (import "${inputs.self}/modules/ttrss" {
      inherit domain inputs;
    })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    "${inputs.self}/modules/mariadb"
  ];
}

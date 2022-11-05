{ custom, hostname }: { pkgs, ... }:
let
  domain = "ttrss.2li.ch";
in
{
  imports = [
    (import "${custom.inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.115";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/nginx-proxy" { inherit custom domain; })
    (import "${custom.inputs.self}/modules/restic-server-mysql-client" {
      path = "/var/lib/ttrss";
      tag = "ttrss";
      time = "23:00";
      inherit custom;
    })
    (import "${custom.inputs.self}/modules/ttrss" { inherit custom domain; })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/mariadb"
  ];
}

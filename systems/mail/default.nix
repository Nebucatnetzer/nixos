{ custom, hostname, inputs }: { pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.123";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      path = "/home/andreas";
      time = "04:30";
      inherit inputs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      domain = "mail.zweili.org"; inherit inputs;
    })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    (import "${inputs.self}/modules/docker-mailserver" { inherit inputs; })
    "${inputs.self}/modules/mariadb"
  ];
}


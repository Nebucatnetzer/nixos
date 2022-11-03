{ custom, hostname, inputs }: { pkgs, ... }:
let
  domain = "git.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.109";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-mysql-client" {
      time = "03:00"; inherit custom hostname inputs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      inherit domain inputs;
    })
    (import "${inputs.self}/modules/gitea" {
      inherit domain inputs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/mariadb"
  ];
}

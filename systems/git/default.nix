{ custom, hostname, inputs }: { pkgs, ... }:
let
  domain = "git.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.109";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-mysql-client" {
      path = "/home/andreas";
      time = "03:00"; inherit inputs;
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      inherit domain inputs;
    })
    (import "${inputs.self}/modules/gitea" {
      inherit domain inputs;
    })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    "${inputs.self}/modules/mariadb"
  ];
}

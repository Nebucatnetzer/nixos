{ custom, hostname }: { pkgs, ... }:
let
  domain = "git.2li.ch";
in
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.109";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/restic-server-mysql-client" {
      path = "/home/andreas";
      time = "00:30"; inherit custom;
    })
    (import "${custom.inputs.self}/modules/nginx-proxy" {
      inherit custom domain;
    })
    (import "${custom.inputs.self}/modules/gitea" {
      inherit custom domain;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/mariadb"
  ];
}

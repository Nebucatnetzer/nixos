{ hostname }: { inputs, pkgs, ... }:
let
  domain = "git.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.109";
      inherit hostname;
    })
    (import "${inputs.self}/modules/restic-client-server-mysql" {
      path = "/home/andreas";
      time = "00:30";
    })
    (import "${inputs.self}/modules/nginx-proxy" {
      inherit domain;
    })
  ];
  services = {
    az-gitea = {
      enable = true;
      domain = domain;
    };
    az-mariadb-for-containers.enable = true;
  };
}

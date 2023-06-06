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
  ];
  services = {
    az-gitea = {
      enable = true;
      domain = domain;
    };
    az-nginx-proxy = {
      enable = true;
      domain = domain;
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = "/home/andreas";
      time = "00:30";
    };
  };
}

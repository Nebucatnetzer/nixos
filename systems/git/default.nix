{ hostname }: { inputs, pkgs, ... }:
let
  domain = "git.2li.ch";
in
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.109";
    };
  };

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

{ hostname }:
{ ... }:
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

  profiles.az-server.enable = true;
  services = {
    az-gitea = {
      enable = true;
      domain = domain;
    };
    az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "${domain}"; }
      ];
    };
    az-nginx-proxy = {
      enable = true;
      domain = domain;
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = "/mnt/server-data";
      time = "00:30";
    };
  };
}

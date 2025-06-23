{ hostname }:
{ config, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.123";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "${config.services.az-nginx-proxy.domain}"; }
      ];
    };
    az-mailserver.enable = true;
    az-nginx-proxy = {
      enable = true;
      domain = "mail.zweili.org";
    };
    az-restic-client-server = {
      enable = true;
      path = "/mnt/server-data";
      tag = "mail";
      time = "01:00";
    };
  };
}

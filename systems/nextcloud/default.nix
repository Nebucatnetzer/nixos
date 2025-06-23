{ hostname }:
{ config, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.103";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "${config.services.az-nextcloud.domain}"; }
      ];
    };
    az-nextcloud = {
      enable = true;
      domain = "nextcloud.2li.ch";
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = "/mnt/server-data";
      time = "02:00";
    };
  };
}

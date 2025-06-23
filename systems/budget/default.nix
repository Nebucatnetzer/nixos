{ hostname }:
{
  config,
  ...
}:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.113";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-firefly.enable = true;
    az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "${config.services.firefly-iii.virtualHost}"; }
      ];
    };
    az-restic-client-server = {
      enable = true;
      path = config.services."firefly-iii".dataDir;
      tag = "firefly";
      time = "22:30";
    };
  };
}

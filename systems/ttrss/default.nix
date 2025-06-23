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
      ip = "10.7.89.115";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "${config.services.freshrss.virtualHost}"; }
        { fqdn = "${config.services.az-rss-bridge.domain}"; }
      ];
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = config.services.freshrss.dataDir;
      tag = "freshrss";
      time = "23:00";
    };
    az-rss-bridge = {
      enable = true;
      domain = "rss-bridge.zweili.org";
    };
    az-freshrss.enable = true;
  };
}

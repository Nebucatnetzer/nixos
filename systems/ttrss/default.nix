{ hostname }:
{
  config,
  inputs,
  pkgs,
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
    az-nginx-proxy = {
      enable = true;
      domain = "rss-bridge.zweili.org";
      port = 8082;
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = config.services.freshrss.dataDir;
      tag = "freshrss";
      time = "23:00";
    };
    az-rss-bridge.enable = true;
    az-freshrss.enable = true;
  };
}

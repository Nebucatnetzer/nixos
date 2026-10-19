{ domain }:
{ config, ... }:
{
  services.nginx.virtualHosts."${domain}".listen = [
    {
      addr = config.az-hosts.gwyn.wgIp;
      port = 80;
    }
  ];
  services.rss-bridge = {
    enable = true;
    config.system.enabled_bridges = [ "*" ];
    virtualHost = domain;
  };
}

{ domain }:
{ config, inputs, ... }:
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
  systemd.services."phpfpm-${config.services.rss-bridge.pool}".serviceConfig =
    import "${inputs.self}/modules/services/lib/deny-private-networks.nix";
}

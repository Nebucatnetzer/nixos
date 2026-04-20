{ config, ... }:
{
  networking.firewall.allowedUDPPorts = [ 53 ];
  services.dnsmasq = {
    enable = true;
    settings = {
      domain-needed = true;
      interface = "wg0";
      no-resolv = true;
      server = [
        config.az-hosts.loki.physicalIp
      ];
    };
  };
}

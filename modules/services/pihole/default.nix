{ config, ... }:
let
  webinterfacePort = 8090;
in
{
  services.unbound = {
    enable = true;
    settings.server = {
      port = 5335;
      interface = [ "127.0.0.1" ];
      do-ip6 = false;
      prefer-ip6 = false;
      harden-glue = true;
      harden-dnssec-stripped = true;
      edns-buffer-size = 1472;
      prefetch = true;
      hide-identity = true;
      hide-version = true;
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts."pihole.vpn.zweili.org" = {
      listen = [
        {
          addr = config.az-hosts.gwyn.wgIp;
          port = 80;
        }
      ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString webinterfacePort}";
      };
    };
  };
  services.pihole-web = {
    enable = true;
    hostName = "pihole.vpn.zweili.org";
    ports = [ webinterfacePort ];
  };

  services.pihole-ftl = {
    enable = true;
    openFirewallDNS = true;
    lists = [
      {
        url = "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts";
        description = "Steven Black's unified adlist";
      }
      {
        url = "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/pro.txt";
        description = "HaGeZi Pro - ads, tracking, and malware";
      }
      {
        url = "https://big.oisd.nl/";
        description = "OISD Big - ads and tracking";
      }
      {
        url = "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/tif.txt";
        description = "HaGeZi Threat Intelligence Feeds - malware and phishing";
      }
    ];
    settings = {
      dns = {
        upstreams = [ "127.0.0.1#5335" ];
        listeningMode = "ALL";
        domainNeeded = true;
        bogusPriv = true;
      };
      misc.dnsmasq_lines = [
        "address=/pihole.vpn.zweili.org/${config.az-hosts.gwyn.wgIp}"
        "address=/librenms.vpn.zweili.org/${config.az-hosts.gwyn.wgIp}"
      ];
      webserver.api.cli_pw = true;
    };
  };
}

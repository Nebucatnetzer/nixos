{ domain, ... }:
{
  networking.firewall.allowedTCPPorts = [
    443
  ];
  security.acme = {
    acceptTerms = true;
    email = "admin+acme@zweili.ch";
  };
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8080";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };
}

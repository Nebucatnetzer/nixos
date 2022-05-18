{ ... }:
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
  };
}

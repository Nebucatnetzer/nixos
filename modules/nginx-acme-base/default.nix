{ ... }:
{
  networking.firewall.allowedTCPPorts = [
    443
  ];
  security.acme = {
    acceptTerms = true;
    defaults.email = "admin+acme@zweili.ch";
  };
  services.nginx = {
    enable = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };
}

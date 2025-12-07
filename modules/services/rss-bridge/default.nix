{ domain }:
{ ... }:
{
  services.az-acme-base = {
    enable = true;
  };
  services.nginx.virtualHosts."${domain}" = {
    enableACME = true;
    forceSSL = true;
    extraConfig = ''
      if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
    '';
  };
  services.rss-bridge = {
    enable = true;
    config.system.enabled_bridges = [ "*" ];
    virtualHost = domain;
  };
}

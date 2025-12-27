{
  domain,
  port ? 8080,
}:
{
  inputs,
  ...
}:
{
  imports = [
    "${inputs.self}/modules/services/nginx-acme-base"
  ];
  services = {
    nginx = {
      appendHttpConfig = ''
        # Disable embedding as a frame
        add_header X-Frame-Options DENY;
      '';
      recommendedProxySettings = true;
      virtualHosts."${domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString port}";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
        extraConfig = ''
          if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
        '';
      };
    };
  };
}

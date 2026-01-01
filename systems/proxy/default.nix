{ hostname }:
{ config, inputs, ... }:
let
  blogPosts = "/var/lib/posts";
  domains = [
    { fqdn = "www.zweili.ch"; }
    { fqdn = "search.zweili.org"; }
    { fqdn = "searxng.zweili.org"; }
  ];
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate" {
    inherit domains;
  };
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.99";
  };
  resticClientServer = import "${inputs.self}/modules/services/restic-client-server";
  searxngHtpasswd = config.age.secrets.searxngHtpasswd.path;
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/haproxy"
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/search"
    raspi4Configs.diskLayouts.singleSdCard
    librenmsCertificateModule
    raspiEthernet
    (resticClientServer {
      path = blogPosts;
      tag = "proxy";
      time = "00:00";
    })
  ];
  age.secrets.searxngHtpasswd = {
    file = "${inputs.self}/scrts/searxng_htpasswd.age";
    mode = "640";
    owner = "root";
    group = config.services.nginx.group;
  };
  services = {
    nginx = {
      commonHttpConfig = ''
        # Add HSTS header with preloading to HTTPS requests.
        # Adding this header to HTTP requests is discouraged
        # Enable CSP for your services.
        #add_header Content-Security-Policy "script-src 'self'; object-src 'none'; base-uri 'none';" always;

        # Minimize information leaked to other domains
        add_header 'Referrer-Policy' 'origin-when-cross-origin';

        # Disable embedding as a frame
        add_header X-Frame-Options DENY;

        # Prevent injection of code in other mime types (XSS Attacks)
        add_header X-Content-Type-Options nosniff;

        # Enable XSS protection of the browser.
        # May be unnecessary when CSP is configured properly (see above)
        add_header X-XSS-Protection "1; mode=block";

        # This might create errors
        proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";
      '';
      recommendedProxySettings = true;
      virtualHosts = {
        "zweili.ch" = {
          serverAliases = [ "www.zweili.ch" ];
          enableACME = true;
          forceSSL = true;
          listen = [
            {
              port = 4433;
              addr = "127.0.0.1";
              ssl = true;
            }
          ];
          root = blogPosts;
        };
        "search.zweili.org" = {
          enableACME = true;
          forceSSL = true;
          listen = [
            {
              port = 4433;
              addr = "127.0.0.1";
              ssl = true;
            }
          ];
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
        };
        "searxng.zweili.org" = {
          enableACME = true;
          forceSSL = true;
          listen = [
            {
              port = 4433;
              addr = "127.0.0.1";
              ssl = true;
            }
          ];
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
          locations."/search" = {
            basicAuthFile = searxngHtpasswd;
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          locations."/stats" = {
            basicAuthFile = searxngHtpasswd;
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
        };
      };
    };
  };
}

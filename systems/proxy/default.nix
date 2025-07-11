{ hostname }:
{ ... }:
let
  blogPosts = "/var/lib/posts";
in
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.99";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-acme-base.enable = true;
    az-haproxy.enable = true;
    az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "www.2li.ch"; }
      ];
    };
    az-restic-client-server = {
      enable = true;
      path = blogPosts;
      tag = "proxy";
      time = "00:00";
    };
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
        "2li.ch" = {
          serverAliases = [ "www.2li.ch" ];
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
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
        };
      };
    };
  };
}

{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.99";
      inherit hostname;
    })
    (import "${inputs.self}/modules/restic-client-server" {
      path = "/home/andreas";
      time = "00:00";
    })
    "${inputs.self}/modules/nginx-acme-base"
    "${inputs.self}/modules/haproxy"
    "${inputs.self}/modules/heimdall"
  ];

  services = {
    az-grav.enable = true;
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
          listen = [{ port = 4433; addr = "127.0.0.1"; ssl = true; }];
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
        };
        "heimdall.2li.ch" = {
          enableACME = true;
          forceSSL = true;
          listen = [{ port = 4433; addr = "127.0.0.1"; ssl = true; }];
          locations."/" = {
            proxyPass = "http://127.0.0.1:8081";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
        };
      };
    };
  };
}

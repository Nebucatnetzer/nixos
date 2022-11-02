{ custom, hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.99";
      inherit hostname inputs pkgs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "00:00"; inherit custom hostname inputs pkgs;
    })
    "${inputs.self}/modules/nginx-acme-base"
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/haproxy"
    "${inputs.self}/modules/heimdall"
    "${inputs.self}/modules/rss-bridge"
  ];

  services.nginx = {
    commonHttpConfig = ''
      # Add HSTS header with preloading to HTTPS requests.
      # Adding this header to HTTP requests is discouraged
      map $scheme $hsts_header {
          https   "max-age=63072000; includeSubdomains; preload";
      }
      add_header Strict-Transport-Security $hsts_header;

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
        listen = [{
          port = 4433;
          addr = "
        127.0.0.1";
          ssl = true;
        }];
        locations."/" = {
          proxyPass = "http://127.0.0.1:8081";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
      };
      "rss-bridge.2li.ch" = {
        enableACME = true;
        forceSSL = true;
        listen = [{ port = 4433; addr = "127.0.0.1"; ssl = true; }];
        locations."/" = {
          proxyPass = "http://127.0.0.1:8082";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
      };
    };
  };
}

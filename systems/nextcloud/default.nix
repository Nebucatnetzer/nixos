{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "nextcloud";
      ip = "10.7.89.103";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "04:00"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/ngix-acme-base"
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

      # Allow embedding from same domain
      add_header X-Frame-Options SAMEORIGIN;

      # Prevent injection of code in other mime types (XSS Attacks)
      add_header X-Content-Type-Options nosniff;

      # Enable XSS protection of the browser.
      # May be unnecessary when CSP is configured properly (see above)
      add_header X-XSS-Protection "1; mode=block";

      # This might create errors
      proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";
    '';
    virtualHosts."nextcloud.2li.ch" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8080";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  }

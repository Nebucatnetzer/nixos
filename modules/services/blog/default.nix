{ ... }:
let
  blogPosts = "/var/lib/posts";
in
{
  services = {
    nginx = {
      commonHttpConfig = ''
        # Add HSTS header.
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
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = {
        "zweili.ch" = {
          serverAliases = [ "www.zweili.ch" ];
          enableACME = true;
          forceSSL = true;
          listen = [
            {
              port = 8433;
              addr = "127.0.0.1";
              ssl = true;
            }
          ];
          root = blogPosts;
        };
      };
    };
  };
}

_: {
  security.acme = {
    acceptTerms = true;
    defaults.email = "admin+acme@zweili.ch";
  };
  services.nginx = {
    enable = true;
    commonHttpConfig = ''
      # Add HSTS header with.
      # Adding this header to HTTP requests is discouraged
      map $scheme $hsts_header {
          https   "max-age=63072000; includeSubdomains;";
      }
      add_header Strict-Transport-Security $hsts_header;

      # Enable CSP for your services.
      #add_header Content-Security-Policy "script-src 'self'; object-src 'none'; base-uri 'none';" always;

      # Minimize information leaked to other domains
      add_header 'Referrer-Policy' 'origin-when-cross-origin';

      # Prevent injection of code in other mime types (XSS Attacks)
      add_header X-Content-Type-Options nosniff;

      # Enable XSS protection of the browser.
      # May be unnecessary when CSP is configured properly (see above)
      add_header X-XSS-Protection "1; mode=block";

      # This might create errors
      proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";

      # Send the access log to journald. Errors go to stderr already (services.nginx.logError).
      access_log syslog:server=unix:/dev/log,tag=nginx_access;
    '';
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    # Reject unknown hostnames instead of serving the first vhost alphabetically.
    virtualHosts."_" = {
      default = true;
      rejectSSL = true;
      locations."/".return = "444";
    };
  };
  services.logrotate.settings.nginx.enable = false;
}

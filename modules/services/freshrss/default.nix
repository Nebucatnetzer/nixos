{ config, inputs, lib, pkgs, ... }:
let cfg = config.services.az-freshrss;
in {
  options = {
    services.az-freshrss.enable = lib.mkEnableOption "Enable FreshRSS.";
  };

  config = lib.mkIf cfg.enable {

    age.secrets.freshrss_db_pass = {
      file = "${inputs.self}/scrts/freshrss_db_pass.age";
      mode = "600";
      owner = config.services.freshrss.user;
      group = config.services.freshrss.user;
    };
    age.secrets.freshrss_user_pass = {
      file = "${inputs.self}/scrts/freshrss_user_pass.age";
      mode = "600";
      owner = config.services.freshrss.user;
      group = config.services.freshrss.user;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
    security.acme = {
      acceptTerms = true;
      defaults.email = "admin+acme@zweili.ch";
    };

    services = {
      az-data-share.enable = true;
      freshrss = {
        enable = true;
        baseUrl = "https://rss.zweili.org";
        database = {
          passFile = config.age.secrets.freshrss_db_pass.path;
          port = 3306;
          type = "mysql";
        };
        defaultUser = "thedoctor";
        passwordFile = config.age.secrets.freshrss_user_pass.path;
        virtualHost = "rss.zweili.org";
      };
      mysql = {
        enable = true;
        package = pkgs.mariadb_110;
        ensureUsers = [{
          name = "freshrss";
          ensurePermissions = { "freshrss.*" = "ALL PRIVILEGES"; };
        }];
        initialDatabases = [{ name = "freshrss"; }];
        settings = {
          mysqld = {
            innodb_file_per_table = 1;
            innodb_buffer_pool_size = "2G";
            read_rnd_buffer_size = "4M";
            sort_buffer_size = "4M";
          };
        };
      };
      nginx = {
        appendHttpConfig = ''
          # Disable embedding as a frame
          add_header X-Frame-Options DENY;
        '';
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

          # Prevent injection of code in other mime types (XSS Attacks)
          add_header X-Content-Type-Options nosniff;

          # Enable XSS protection of the browser.
          # May be unnecessary when CSP is configured properly (see above)
          add_header X-XSS-Protection "1; mode=block";

          # This might create errors
          proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";
        '';
        recommendedOptimisation = true;
        recommendedTlsSettings = true;
        virtualHosts."rss.zweili.org" = {
          enableACME = true;
          forceSSL = true;
        };
      };
    };
  };
}


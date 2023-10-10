{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.services.az-roundcube;
  twofactor_gauthenticator = pkgs.roundcubePlugins.roundcubePlugin rec {
    pname = "twofactor_gauthenticator";
    version = "v2023-09-13";
    src = pkgs.fetchzip {
      url = "https://github.com/alexandregz/twofactor_gauthenticator/archive/95f7b5a8c17db9e9f089656e06ae6703d139e76e.zip";
      hash = "sha256-BsGLLJ4YP+y+DFSwbSEx8Hqh9wfJAS/V6iLQEym1W10=";
    };
  };
in
{
  options = {
    services.az-roundcube.enable = lib.mkEnableOption "My configuration to enable roundcube.";
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
    security.acme = {
      acceptTerms = true;
      defaults.email = "admin+acme@zweili.ch";
    };
    services = {
      az-postgresql.enable = true;
      nginx = {
        enable = true;
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
      };
      postgresql = {
        ensureDatabases = [ "roundcube" ];
        ensureUsers = [{
          name = "roundcube";
          ensurePermissions = {
            "DATABASE roundcube" = "ALL PRIVILEGES";
          };
        }];
      };
      roundcube = {
        database = {
          username = "roundcube";
        };
        # dicts = with pkgs.aspellDicts; [ en de ];
        enable = true;
        extraConfig = ''
          $config['imap_host'] = array(
            'ssl://mail.zweili.org:993' => "Zweili",
          );
          $config['username_domain'] = array(
            'mail.zweili.org' => 'zweili.ch',
          );
          $config['x_frame_options'] = false;
          $config['smtp_host'] = "ssl://mail.zweili.org:465";
          $config['smtp_user'] = "%u";
          $config['smtp_pass'] = "%p";
        '';
        hostName = "mail.zweili.org";
        maxAttachmentSize = 25;
        plugins = [ "carddav" "persistent_login" "twofactor_gauthenticator" ];
        package = pkgs.roundcube.withPlugins (plugins:
          with plugins; [ carddav persistent_login twofactor_gauthenticator ]
        );
      };
    };
  };
}

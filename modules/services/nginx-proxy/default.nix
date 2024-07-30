{ config, lib, ... }:
let
  cfg = config.services.az-nginx-proxy;
in
{
  options = {
    services.az-nginx-proxy = {
      enable = lib.mkEnableOption "Enable Nginx proxy, mainly to provide SSL.";
      domain = lib.mkOption {
        type = lib.types.str;
        description = "The domain the service is being run from.";
      };
      port = lib.mkOption {
        type = lib.types.number;
        description = "The port FPM listens on.";
        default = 8080;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services = {
      az-acme-base.enable = true;
      nginx = {
        appendHttpConfig = ''
          # Disable embedding as a frame
          add_header X-Frame-Options DENY;
        '';
        recommendedProxySettings = true;
        virtualHosts."${cfg.domain}" = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
        };
      };
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-nginx-fpm;
in
{
  options = {
    services.az-nginx-fpm = {
      enable = lib.mkEnableOption "Enable Nginx with config for FPM in a container.";
      dataDir = lib.mkOption {
        type = lib.types.str;
        description = "The directory where the application lives on the host.";
      };
      documentRoot = lib.mkOption {
        type = lib.types.str;
        description = "The directory where the FPM expects your code to be.";
        default = "/var/www/html";
      };
      domain = lib.mkOption {
        type = lib.types.str;
        description = "The domain the service is being run from.";
      };
      port = lib.mkOption {
        type = lib.types.number;
        description = "The port FPM listens on.";
        default = 9000;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services = {
      az-acme-base.enable = true;
      nginx = {
        appendHttpConfig = ''
          index index.php;
        '';
        virtualHosts."${cfg.domain}" = {
          enableACME = true;
          forceSSL = true;
          root = cfg.dataDir;
          locations = {
            "~ \\.php$" = {
              extraConfig = ''
                fastcgi_split_path_info ^(.+?\.php)(/.*)$;
                include ${pkgs.nginx}/conf/fastcgi_params;
                include ${pkgs.nginx}/conf/fastcgi.conf;
                fastcgi_param  SCRIPT_FILENAME  ${cfg.documentRoot}$fastcgi_script_name;
                fastcgi_index index.php;
                fastcgi_pass 127.0.0.1:${toString cfg.port};
              '';
            };
          };
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
        };
      };
    };
  };
}

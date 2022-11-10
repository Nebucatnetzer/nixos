{ custom, domain }: { config, ... }:
{
  imports = [
    (import "${custom.inputs.self}/modules/nginx-fpm" {
      dataDir = "/var/lib/ttrss/html";
      inherit custom domain;
    })
    "${custom.inputs.self}/modules/postgresql"
  ];
  age.secrets.ttrssEnv.file = "${custom.inputs.self}/scrts/ttrss_env.age";

  services.postgresql = {
    authentication = "host ttrssdb ttrss 172.16.0.0/12 scram-sha-256";
    ensureDatabases = [ "ttrssdb" ];
    ensureUsers = [{
      name = "ttrss";
      ensurePermissions = {
        "DATABASE ttrssdb " = "ALL PRIVILEGES";
      };
    }];
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."ttrss" = {
      image = "ghcr.io/nebucatnetzer/tt-rss-aarch64/ttrss-fpm-pgsql-static";
      autoStart = true;
      environment = {
        TZ = "Europe/Zurich";
        TTRSS_DB_USER = "ttrss";
        TTRSS_DB_NAME = "ttrssdb";
        TTRSS_DB_HOST = "host.docker.internal";
        TTRSS_SELF_URL_PATH = "https://${domain}/tt-rss";
        TTRSS_SESSION_COOKIE_LIFETIME = "604800";
      };
      environmentFiles = [ config.age.secrets.ttrssEnv.path ];
      ports = [
        "9000:9000"
      ];
      volumes = [
        "/var/lib/ttrss/html:/var/www/html"
      ];
      extraOptions = [ "--add-host=host.docker.internal:host-gateway" ];
    };
  };

  services.nginx.virtualHosts."${domain}".locations = {
    "/".extraConfig = ''
      try_files $uri $uri/ = 404;
    '';
    "/tt-rss/cache".extraConfig = ''
      aio threads;
      internal;
    '';
    "/tt-rss/backups".extraConfig = ''
      internal;
    '';
  };
}

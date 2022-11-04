{ custom, domain }: { config, pkgs, ... }:
let
  domain = "test.2li.ch";
in
{
  imports = [
    (import "${custom.inputs.self}/modules/nginx-fpm" {
      dataDir = "/mnt/data/ttrss/app";
      inherit custom domain pkgs;
    })
    "${custom.inputs.self}/modules/data-share"
    "${custom.inputs.self}/modules/postgresql"
  ];
  age.secrets.ttrssEnv.file = "${custom.inputs.self}/scrts/ttrss_env.age";

  services.postgresql = {
    ensureDatabases = [ "ttrssdb" ];
    initialScript = pkgs.writeText "postgresql-initScript" ''
      CREATE ROLE ttrss WITH LOGIN PASSWORD 'ttrss' CREATEDB;
      GRANT ALL PRIVILEGES ON DATABASE ttrssdb TO ttrss;
    '';
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."ttrss" = {
      image = "ghcr.io/nebucatnetzer/tt-rss-aarch64/ttrss-fpm-pgsql-static";
      autoStart = false;
      environment = {
        TZ = "Europe/Zurich";
        TTRSS_DB_USER = "ttrss";
        TTRSS_DB_NAME = "ttrssdb";
        TTRSS_DB_PASS = "ttrss";
        TTRSS_DB_HOST = "host.docker.internal";
        TTRSS_SELF_URL_PATH = "https://${domain}";
      };
      environmentFiles = [ config.age.secrets.ttrssEnv.path ];
      ports = [
        "8080:80"
      ];
      volumes = [
        "/var/lib/ttrss/config:/config"
      ];
      extraOptions = [ "--add-host=host.docker.internal:host-gateway" ];
    };
  };
}

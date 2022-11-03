{ domain, inputs }: { config, pkgs, ... }:
let
  domain = "test.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/modules/nginx-fpm" {
      dataDir = "/mnt/data/ttrss/app";
      inherit domain inputs pkgs;
    })
    "${inputs.self}/modules/data-share"
    "${inputs.self}/modules/postgresql"
  ];

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
        TTRSS_SELF_URL_PATH = "https://test.2li.ch";
      };
      # environmentFiles = "";
      ports = [
        "8080:80"
      ];
      volumes = [
        "/home/andreas/ttrss/config:/config"
      ];
    };
  };
}

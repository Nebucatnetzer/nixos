{ domain, inputs }: { config, ... }:
{
  age.secrets.ttrssEnv.file = "${inputs.self}/scrts/ttrss_env.age";

  virtualisation.oci-containers = {
    backend = "docker";
    containers."ttrss" = {
      image = "registry.gitlab.com/lunik1/docker-tt-rss";
      autoStart = true;
      environment = {
        PUID = "1000";
        PGID = "1000";
        TZ = "Europe/Zurich";
        TTRSS_DB_TYPE = "mysql";
        TTRSS_DB_USER = "ttrss";
        TTRSS_DB_NAME = "ttrssdb";
        TTRSS_DB_PORT = "3306";
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

{ domain }: { config, inputs, ... }:
let
  ttrssEnvironment = {
    TZ = "Europe/Zurich";
    TTRSS_DB_USER = "ttrss";
    TTRSS_DB_NAME = "ttrssdb";
    TTRSS_DB_HOST = "host.docker.internal";
    TTRSS_SELF_URL_PATH = "https://${domain}/tt-rss";
    TTRSS_SESSION_COOKIE_LIFETIME = "604800";
    TTRSS_PLUGINS = "af_comics, af_readability, auth_internal, hotkeys_swap_jk, nginx_xaccel";
  };
  # https://github.com/Nebucatnetzer/tt-rss-aarch64/pkgs/container/tt-rss-aarch64%2Fttrss-fpm-pgsql-static/versions
  ttrssImage = "ghcr.io/nebucatnetzer/tt-rss-aarch64/ttrss-fpm-pgsql-static@sha256:4842ca145ad3d57b1b627fdf9ea4349aeeda9e31134deee1e6a64694f6825754";
  ttrssService = "${config.virtualisation.oci-containers.backend}-ttrss";
in
{
  age.secrets.ttrssEnv.file = "${inputs.self}/scrts/ttrss_env.age";

  services = {
    az-docker.enable = true;
    az-nginx-fpm = {
      enable = true;
      dataDir = "/var/lib/ttrss/html";
      domain = domain;
    };
    az-postgresql.enable = true;
    nginx.virtualHosts."${domain}".locations = {
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
    postgresql = {
      authentication = "host ttrssdb ttrss 172.16.0.0/12 scram-sha-256";
      ensureDatabases = [ "ttrssdb" ];
      ensureUsers = [{
        name = "ttrss";
        ensurePermissions = {
          "DATABASE ttrssdb " = "ALL PRIVILEGES";
        };
      }];
    };
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."ttrss" = {
      image = ttrssImage;
      autoStart = true;
      environment = ttrssEnvironment;
      environmentFiles = [ config.age.secrets.ttrssEnv.path ];
      ports = [
        "9000:9000"
      ];
      volumes = [
        "/var/lib/ttrss/html:/var/www/html"
        "/etc/localtime:/etc/localtime:ro"
      ];
      extraOptions = [
        "--add-host=host.docker.internal:host-gateway"
        "--log-opt=tag='ttrss'"
      ];
    };
    containers."backup" = {
      image = ttrssImage;
      autoStart = true;
      environment = ttrssEnvironment;
      environmentFiles = [ config.age.secrets.ttrssEnv.path ];
      volumes = [
        "/var/lib/ttrss/html:/var/www/html"
        "/var/lib/ttrss/backup:/backup"
        "/etc/localtime:/etc/localtime:ro"
      ];
      cmd = [ "/opt/tt-rss/dcron.sh" "-f" ];
      extraOptions = [
        "--add-host=host.docker.internal:host-gateway"
        "--log-opt=tag='ttrss-backup'"
      ];
    };
    containers."updater" = {
      image = ttrssImage;
      autoStart = true;
      environment = ttrssEnvironment;
      environmentFiles = [ config.age.secrets.ttrssEnv.path ];
      volumes = [
        "/var/lib/ttrss/html:/var/www/html"
        "/etc/localtime:/etc/localtime:ro"
      ];
      cmd = [ "/opt/tt-rss/updater.sh" ];
      dependsOn = [ "ttrss" ];
      extraOptions = [
        "--add-host=host.docker.internal:host-gateway"
        "--log-opt=tag='ttrss-updater'"
      ];
    };
  };

  systemd.services.${ttrssService}.after = [ "nginx.service" ];
  systemd.services.postgresql.after = [ "${ttrssService}.service" ];
}





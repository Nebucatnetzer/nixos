{ dataDir, domain }:
{
  config,
  inputs,
  ...
}:
let
  port = 3000;
in
{
  imports = [
    "${inputs.self}/modules/services/docker"
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/mariadb-for-containers"
  ];
  age.secrets.giteaEnv.file = "${inputs.self}/scrts/gitea_env.age";

  services = {
    nginx = {
      appendHttpConfig = ''
        # Disable embedding as a frame
        add_header X-Frame-Options DENY;
      '';
      recommendedProxySettings = true;
      virtualHosts."${domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString port}";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
        extraConfig = ''
          if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
        '';
      };
    };
  };

  services.snmpd.configText = ''
    # monitor gitea
    proc gitea
  '';
  services = {
    mysql.settings = {
      mysql = {
        default-character-set = "utf8mb4";
      };
      mysqld = {
        collation-server = "utf8mb4_unicode_ci";
        init-connect = "SET NAMES utf8mb4";
        character-set-server = "utf8mb4";
        innodb_file_per_table = 1;
        innodb_buffer_pool_size = "2G";
        read_rnd_buffer_size = "4M";
        sort_buffer_size = "4M";
      };
    };
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."gitea" = {
      # https://blog.gitea.io/
      # https://hub.docker.com/r/gitea/gitea/tags
      image = "docker.io/gitea/gitea:1.26.1@sha256:d8667667b4ccbd1f67b86a376bffcc0a17b16cf71309ed04e3918231776d47dd";
      autoStart = true;
      environment = {
        PUID = "1000";
        PGID = "100";
        DB_TYPE = "mysql";
        DB_HOST = "host.docker.internal:3306";
        DB_NAME = "giteadb";
        DB_USER = "gitea";
        ROOT_URL = "${domain}";
        INSTALL_LOCK = "true";
        DISABLE_REGISTRATION = "true";
      };
      environmentFiles = [ config.age.secrets.giteaEnv.path ];
      ports = [
        "2222:22"
        "3000:3000"
      ];
      volumes = [
        "/etc/timezone:/etc/timezone:ro"
        "/etc/localtime:/etc/localtime:ro"
        "${dataDir}:/data"
      ];
      extraOptions = [
        "--add-host=host.docker.internal:host-gateway"
        "--log-opt=tag='gitea'"
      ];
    };
  };
}

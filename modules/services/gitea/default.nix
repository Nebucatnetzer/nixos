{ dataDir, domain }:
{
  config,
  inputs,
  ...
}:
let
  botBlockRegex = import "${inputs.self}/modules/services/nginx/lib/nginx-bot-block.nix";
  mariadbTuning = import "${inputs.self}/modules/services/mariadb/lib/mariadb-tuning.nix";
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
          if ($http_user_agent ~* "${botBlockRegex}") { return 403; }
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
      }
      // mariadbTuning;
    };
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."gitea" = {
      # https://blog.gitea.io/
      # https://hub.docker.com/r/gitea/gitea/tags
      image = "docker.io/gitea/gitea:1.26.2@sha256:7d13848af12645600a5f9d93ee2560daa9c6fa6b5b859b7bff3a5e1c0b661031";
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

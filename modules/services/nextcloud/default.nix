{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.services.az-nextcloud;
  nextcloudEnvironment = {
    MYSQL_DATABASE = "nextcloud";
    MYSQL_USER = "nextcloud";
    MYSQL_HOST = "172.17.0.1";
    NEXTCLOUD_TRUSTED_DOMAINS = "${cfg.domain} ${config.networking.hostName}.2li.local 10.7.89.103";
    REDIS_HOST = "redis";
    SMTP_HOST = "mail.infomaniak.com";
    SMTP_SECURE = "ssl";
    SMTP_PORT = "465";
  };
  networkName = "nextcloud";
  # https://github.com/Nebucatnetzer/nextcloud-smb
  nextcloudImage = "ghcr.io/nebucatnetzer/nextcloud-smb/nextcloud-smb:27.1.4@sha256:1e0f9de2938d9d6084d48ed7a3ee6fbc23485afa4a5ebe6766da671e6105efbc";
  nextcloudService = "${config.virtualisation.oci-containers.backend}-nextcloud";
  cronService = "${config.virtualisation.oci-containers.backend}-cron";
in
{
  options = {
    services.az-nextcloud.enable = lib.mkEnableOption "Enable Nextcloud running in a container.";
    services.az-nextcloud.domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain Nextcloud is being run from.";
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets.nextcloudEnv.file = "${inputs.self}/scrts/nextcloud_env.age";

    services = {
      az-acme-base.enable = true;
      az-docker.enable = true;
      az-mariadb-for-containers.enable = true;
      mysql.settings = {
        mysqld = {
          innodb_file_per_table = 1;
          innodb_buffer_pool_size = "2G";
          read_rnd_buffer_size = "4M";
          sort_buffer_size = "4M";
        };
      };
      nginx = {
        appendHttpConfig = ''
          # Allow embedding from same domain
          add_header X-Frame-Options SAMEORIGIN;
        '';
        clientMaxBodySize = "20G";
        virtualHosts."${cfg.domain}" = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            # Required for large downloads
            proxy_buffering off;
          '';
        };
      };
    };

    virtualisation.oci-containers = {
      backend = "docker";
      containers."nextcloud" = {
        image = nextcloudImage;
        autoStart = true;
        environment = nextcloudEnvironment;
        environmentFiles = [ config.age.secrets.nextcloudEnv.path ];
        volumes = [
          "${inputs.self}/modules/services/nextcloud/custom-php.ini:/usr/local/etc/php/conf.d/zzz-custom.ini:ro"
          "/etc/localtime:/etc/localtime:ro"
        ];
        dependsOn = [ "redis" ];
        extraOptions = [
          ''--mount=type=volume,source=nextcloud_data,target=/var/www/html,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/nextcloud/data,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
          "--add-host=host.docker.internal:host-gateway"
          "--net=${networkName}"
          "--log-opt=tag='nextcloud'"
        ];
      };
      containers."nginx" = {
        image = "nginx:1.22.1";
        autoStart = true;
        ports = [
          "8080:80"
        ];
        volumes = [
          "${inputs.self}/modules/services/nextcloud/nginx.conf:/etc/nginx/nginx.conf:ro"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          ''--mount=type=volume,source=nextcloud_data,target=/var/www/html,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/nextcloud/data,"volume-opt=o=addr=10.7.89.108,ro,nfsvers=4.0,nolock,hard,noatime"''
          "--net=${networkName}"
          "--log-opt=tag='nextcloud-nginx'"
        ];
      };
      containers."cron" = {
        image = nextcloudImage;
        autoStart = true;
        environment = nextcloudEnvironment;
        environmentFiles = [ config.age.secrets.nextcloudEnv.path ];
        entrypoint = "/cron.sh";
        dependsOn = [ "redis" ];
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          ''--mount=type=volume,source=nextcloud_data,target=/var/www/html,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/nextcloud/data,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
          "--add-host=host.docker.internal:host-gateway"
          "--net=nextcloud"
          "--log-opt=tag='nextcloud-cron'"
        ];
      };
      containers."redis" = {
        image = "redis:alpine";
        autoStart = true;
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          "--net=${networkName}"
          "--log-opt=tag='redis'"
        ];
      };
    };
    system.activationScripts.makeDockerNetwork = ''
      ${pkgs.docker}/bin/docker network ls | ${pkgs.gnugrep}/bin/grep ${networkName} || ${pkgs.docker}/bin/docker network create ${networkName}
    '';

    environment.shellAliases = {
      occ = ''${pkgs.docker}/bin/docker exec -u www-data nextcloud php occ'';
    };
    systemd.services.${nextcloudService}.after = [ "mysql.service" "nginx.service" ];
    systemd.services.${cronService}.after = [ "mysql.service" ];
  };
}

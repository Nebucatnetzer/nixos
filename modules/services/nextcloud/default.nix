{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-nextcloud;
  cronService = "${config.virtualisation.oci-containers.backend}-cron";
  nextcloudEnvironment = {
    MYSQL_DATABASE = "nextcloud";
    MYSQL_USER = "nextcloud";
    MYSQL_HOST = "172.17.0.1";
    NEXTCLOUD_TRUSTED_DOMAINS = "${cfg.domain} ${config.networking.hostName}.2li.local 10.7.89.103";
    REDIS_HOST = "redis";
    SMTP_HOST = "mail.zweili.org";
    SMTP_SECURE = "ssl";
    SMTP_PORT = "465";
  };
  networkName = "nextcloud";
  # https://github.com/Nebucatnetzer/nextcloud-smb
  nextcloudImage = "ghcr.io/nebucatnetzer/nextcloud-smb/nextcloud-smb:29.0.1@sha256:2a10055c97d28d5264ff6d66639f3c8b7996e2fcd42e76230baecabeb3edfefc";
  nextcloudService = "${config.virtualisation.oci-containers.backend}-nextcloud";
  volumePath = "/mnt/server-data/nextcloud";
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

    fileSystems."${volumePath}" = {
      device = "10.7.89.108:server_data/nextcloud/data";
      fsType = "nfs";
      options = [
        "hard"
        "noatime"
        "rw"
      ];
    };
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
          "${volumePath}:/var/www/html"
        ];
        dependsOn = [ "redis" ];
        extraOptions = [
          "--add-host=host.docker.internal:host-gateway"
          "--net=${networkName}"
          "--log-opt=tag='nextcloud'"
        ];
      };
      containers."nginx" = {
        image = "nginx:1.25.3";
        autoStart = true;
        ports = [ "8080:80" ];
        volumes = [
          "${inputs.self}/modules/services/nextcloud/nginx.conf:/etc/nginx/nginx.conf:ro"
          "/etc/localtime:/etc/localtime:ro"
          "${volumePath}:/var/www/html"
        ];
        extraOptions = [
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
          "${volumePath}:/var/www/html"
        ];
        extraOptions = [
          "--add-host=host.docker.internal:host-gateway"
          "--net=nextcloud"
          "--log-opt=tag='nextcloud-cron'"
        ];
      };
      containers."redis" = {
        image = "redis:alpine";
        autoStart = true;
        volumes = [ "/etc/localtime:/etc/localtime:ro" ];
        extraOptions = [
          "--net=${networkName}"
          "--log-opt=tag='redis'"
        ];
      };
    };
    system.activationScripts.makeDockerNetwork = ''
      ${pkgs.docker}/bin/docker network ls | ${pkgs.gnugrep}/bin/grep ${networkName} || ${pkgs.docker}/bin/docker network create ${networkName}
    '';

    systemd.services.nextcloud-previews = {
      serviceConfig = {
        Type = "oneshot";
      };
      script = ''
        ${pkgs.docker}/bin/docker exec -u www-data nextcloud php occ preview:pre-generate
      '';
    };
    systemd.timers.nextcloud-previews = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "*:0/10";
    };

    environment.shellAliases = {
      occ = "${pkgs.docker}/bin/docker exec -u www-data nextcloud php occ";
    };
    systemd.services.${nextcloudService} = {
      after = [
        "mysql.service"
        "nginx.service"
      ];
      wants = [
        "mysql.service"
        "nginx.service"
      ];
    };
    systemd.services.${cronService} = {
      after = [ "mysql.service" ];
      wants = [ "mysql.service" ];
    };
  };
}

{ custom, domain }: { config, pkgs, ... }:
let
  nextcloudEnvironment = {
    MYSQL_DATABASE = "nextcloud";
    MYSQL_USER = "nextcloud";
    MYSQL_HOST = "172.17.0.1";
    NEXTCLOUD_TRUSTED_DOMAINS = "${domain} ${config.networking.hostName}.2li.local 10.7.89.103";
    REDIS_HOST = "redis";
    SMTP_HOST = "mail.infomaniak.com";
    SMTP_SECURE = "ssl";
    SMTP_PORT = "465";
  };
  networkName = "nextcloud";
  nextcloudImage = "ghcr.io/nebucatnetzer/nextcloud-smb/nextcloud-smb:25.0.2@sha256:d77223c2a89b37d27bce3e88fe229026146675242949050e743dd8155c53fdc3";
  nextcloudService = "${config.virtualisation.oci-containers.backend}-nextcloud";
  cronService = "${config.virtualisation.oci-containers.backend}-cron";
in
{
  age.secrets.nextcloudEnv.file = "${custom.inputs.self}/scrts/nextcloud_env.age";

  services.mysql.settings = {
    mysqld = {
      innodb_file_per_table = 1;
      innodb_buffer_pool_size = "2G";
      read_rnd_buffer_size = "4M";
      sort_buffer_size = "4M";
    };
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."nextcloud" = {
      image = nextcloudImage;
      autoStart = true;
      environment = nextcloudEnvironment;
      environmentFiles = [ config.age.secrets.nextcloudEnv.path ];
      ports = [
        "8080:80"
      ];
      volumes = [
        "${custom.inputs.self}/modules/nextcloud/custom-php.ini:/usr/local/etc/php/conf.d/zzz-custom.ini"
        "/etc/localtime:/etc/localtime:ro"
      ];
      dependsOn = [ "redis" ];
      extraOptions = [
        ''--mount=type=volume,source=nextcloud_data,target=/var/www/html,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/nextcloud/data,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        "--add-host=host.docker.internal:host-gateway"
        "--net=${networkName}"
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
}

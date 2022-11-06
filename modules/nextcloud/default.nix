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
  nextcloudImage = "ghcr.io/nebucatnetzer/nextcloud-smb/nextcloud-smb:25.0.1@sha256:331545c8213f4774d904e662111d0725d0c625e75bbcc7cc1e74b60b1b31d135";
in
{
  age.secrets.nextcloudEnv.file = "${custom.inputs.self}/scrts/nextcloud_env.age";

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
      extraOptions = [
        ''--mount=type=volume,source=nextcloud_data,target=/var/www/html,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/nextcloud/data,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        "--add-host=host.docker.internal:host-gateway"
        "--net=nextcloud"
      ];
    };
    containers."redis" = {
      image = "redis:alpine";
      autoStart = true;
      extraOptions = [
        "--net=${networkName}"
      ];
    };
  };
  system.activationScripts.makeDokerNetwork = ''
    ${pkgs.docker}/bin/docker network ls | ${pkgs.gnugrep}/bin/grep ${networkName} || {pkgs.docker}/bin/docker network create ${networkName}
  '';

}
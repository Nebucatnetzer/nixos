{ config, inputs, lib, ... }:
let
  cfg = config.services.az-gitea;
in
{
  options = {
    services.az-gitea.enable = lib.mkEnableOption "Enable Gitea in Docker.";
    services.az-gitea.domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain Gitea is being run from.";
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets.giteaEnv.file = "${inputs.self}/scrts/gitea_env.age";

    services = {
      az-docker.enable = true;
      az-mariadb-for-containers.enable = true;
    };

    virtualisation.oci-containers = {
      backend = "docker";
      containers."gitea" = {
        # https://blog.gitea.io/
        # https://hub.docker.com/r/gitea/gitea/tags
        image = "gitea/gitea:1.20.4";
        autoStart = true;
        environment = {
          PUID = "1000";
          PGID = "100";
          DB_TYPE = "mysql";
          DB_HOST = "host.docker.internal:3306";
          DB_NAME = "giteadb";
          DB_USER = "gitea";
          ROOT_URL = "${cfg.domain}";
          INSTALL_LOCK = "true";
          DISABLE_REGISTRATION = "true";
        };
        environmentFiles = [ config.age.secrets.giteaEnv.path ];
        ports = [
          "2222:22"
          "8080:3000"
        ];
        volumes = [
          "/etc/timezone:/etc/timezone:ro"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          ''--mount=type=volume,source=gitea_data,target=/data,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/gitea/data,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
          "--add-host=host.docker.internal:host-gateway"
          "--log-opt=tag='gitea'"
        ];
      };
    };
  };
}

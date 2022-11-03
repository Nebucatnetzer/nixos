{ domain, inputs }: { config, ... }:
{
  age.secrets.giteaEnv.file = "${inputs.self}/scrts/gitea_env.age";

  virtualisation.oci-containers = {
    backend = "docker";
    containers."gitea" = {
      image = "gitea/gitea:1.17.3";
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
        "8080:3000"
      ];
      volumes = [
        "/etc/timezone:/etc/timezone:ro"
        "/etc/localtime:/etc/localtime:ro"
      ];
      extraOptions = [
        ''--mount=type=volume,source=gitea_data,target=/data,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/gitea/data,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        "--add-host=host.docker.internal:host-gateway"
      ];
    };
  };
}

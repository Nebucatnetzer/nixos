{ domain }:
{
  config,
  inputs,
  ...
}:
let
  volumePath = "/mnt/server-data/gitea";
in
{
  imports = [
    "${inputs.self}/modules/services/docker"
    "${inputs.self}/modules/services/mariadb-for-containers"
  ];
  age.secrets.giteaEnv.file = "${inputs.self}/scrts/gitea_env.age";

  fileSystems."${volumePath}" = {
    device = "10.7.89.108:server_data/gitea/data";
    fsType = "nfs";
    options = [
      "hard"
      "noatime"
      "rw"
    ];
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
      image = "docker.io/gitea/gitea:1.25.2@sha256:534428e78fc00d3ac8647f3467a3f91252acf23a46ea0c872f03191e3c878f7d";
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
        "${volumePath}:/data"
      ];
      extraOptions = [
        "--add-host=host.docker.internal:host-gateway"
        "--log-opt=tag='gitea'"
      ];
    };
  };
}

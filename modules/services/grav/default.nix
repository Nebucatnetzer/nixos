{ config, lib, ... }:
let
  cfg = config.services.az-grav;
  volumePath = "/mnt/server-data/grav";
in
{
  options = {
    services.az-grav.enable = lib.mkEnableOption "Enable Grav.";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;

    fileSystems."${volumePath}" = {
      device = "10.7.89.108:server_data/grav";
      fsType = "nfs";
      options = [
        "hard"
        "noatime"
        "rw"
      ];
    };
    virtualisation.oci-containers = {
      backend = "docker";
      containers."grav" = {
        # https://fleet.linuxserver.io/image?name=linuxserver/grav
        image = "lscr.io/linuxserver/grav:1.7.46@sha256:c7a25295535b7c4736ff8ed5d34da484e5cffa57683cc7260609661099119890";
        autoStart = true;
        environment = {
          TZ = "Europe/Zurich";
          PUID = "100";
          PGID = "101";
        };
        ports = [ "8080:80" ];
        volumes = [
          "/etc/timezone:/etc/timezone:ro"
          "/etc/localtime:/etc/localtime:ro"
          "${volumePath}:/config"
        ];
        extraOptions = [ "--log-opt=tag='grav'" ];
      };
    };
  };
}

{ config, lib, ... }:
let
  cfg = config.services.az-heimdall;
  volumePath = "/mnt/server-data/heimdall";
in
{
  options = {
    services.az-heimdall.enable = lib.mkEnableOption "Enable Heimdall";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;

    fileSystems."${volumePath}" = {
      device = "10.7.89.108:server_data/heimdall";
      fsType = "nfs";
      options = [
        "hard"
        "noatime"
        "rw"
      ];
    };
    virtualisation.oci-containers = {
      backend = "docker";
      containers."heimdall" = {
        # https://fleet.linuxserver.io/image?name=linuxserver/heimdall
        image = "linuxserver/heimdall:2.5.8@sha256:a81a314a2d021e43d91ee88830974546239ca30851f4556bff80518870cca8e7";
        autoStart = true;
        environment = {
          TZ = "Europe/Zurich";
          PUID = "1000";
          PGID = "100";
        };
        ports = [ "8081:80" ];
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
          "${volumePath}:/config"
        ];
        extraOptions = [ "--log-opt=tag='heimdall'" ];
      };
    };
  };
}

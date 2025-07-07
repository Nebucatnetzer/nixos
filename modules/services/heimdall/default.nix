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
        image = "lscr.io/linuxserver/heimdall:2.6.3@sha256:aefaf6ffc2f3925f09b2b898b1544ed58cc5f4b504f3163865c80afae78b4fe2";
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

{ config, lib, ... }:
let
  cfg = config.services.az-heimdall;
in
{
  options = {
    services.az-heimdall.enable = lib.mkEnableOption "Enable Heimdall";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;

    virtualisation.oci-containers = {
      backend = "docker";
      containers."heimdall" = {
        # https://fleet.linuxserver.io/image?name=linuxserver/heimdall
        image = "linuxserver/heimdall:2.5.6@sha256:768532e72631397a05dbc9f2b69ae69dcbf65e295fab7f7ebba27f6e7723bc41";
        autoStart = true;
        environment = {
          TZ = "Europe/Zurich";
          PUID = "1000";
          PGID = "100";
        };
        ports = [
          "8081:80"
        ];
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          ''--mount=type=volume,source=heimdall,target=/config,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/heimdall,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
          "--log-opt=tag='heimdall'"
        ];
      };
    };
  };
}


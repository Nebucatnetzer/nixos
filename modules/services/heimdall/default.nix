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
        image = "linuxserver/heimdall:2.5.7@sha256:850fe819e5b5d5835ef08ad888cf4b0f445b10260b628628a42047c00c4c68f1";
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


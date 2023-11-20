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
        image = "linuxserver/heimdall:2.5.8@sha256:ed4b4e405b2e5fec3131d28bd5325be0edc8dc1444679eae951b22b84548f56b";
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


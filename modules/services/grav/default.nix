{ config, lib, ... }:
let
  cfg = config.services.az-grav;
in
{
  options = {
    services.az-grav.enable = lib.mkEnableOption "Enable Grav.";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;

    virtualisation.oci-containers = {
      backend = "docker";
      containers."grav" = {
        # https://fleet.linuxserver.io/image?name=linuxserver/grav
        image = "lscr.io/linuxserver/grav:1.7.43@sha256:e9db6762bc03e90a5838e7c9202e3625caa5b7694819a0906ac3bdd7d0ed333b";
        autoStart = true;
        environment = {
          TZ = "Europe/Zurich";
          PUID = "100";
          PGID = "101";
        };
        ports = [
          "8080:80"
        ];
        volumes = [
          "/etc/timezone:/etc/timezone:ro"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          ''--mount=type=volume,source=grav,target=/config,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/grav,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
          "--log-opt=tag='grav'"
        ];
      };
    };
  };
}


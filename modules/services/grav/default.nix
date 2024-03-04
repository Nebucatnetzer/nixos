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
        image = "lscr.io/linuxserver/grav:1.7.44@sha256:3b82a69d1594c02023d4ec3907fea18a8bf287491b109ed2b7f1a2ed2907576e";
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

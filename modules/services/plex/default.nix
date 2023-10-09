{ config, inputs, lib, ... }:
let
  cfg = config.services.az-plex;
in
{
  options = {
    services.az-plex.enable = lib.mkEnableOption "Enable Plex running in Docker";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.plexClaim.file = "${inputs.self}/scrts/plex_claim.age";
    networking = {
      firewall.allowedTCPPorts = [
        32400 # Web Interface/ Remote Access
      ];
      firewall.allowedUDPPorts = [
        1900 # DLNA
        5353 # Bonjour/Avahi
        32410 # GDM network discovery
        32412 # GDM network discovery
        32413 # GDM network discovery
        32414 # GDM network discovery
        32469 # Plex DLNA Server
      ];
    };

    services.az-docker.enable = true;

    virtualisation.oci-containers = {
      backend = "docker";
      containers."plex" = {
        autoStart = true;
        # https://fleet.linuxserver.io/image?name=linuxserver/plex
        image = "lscr.io/linuxserver/plex:1.32.6@sha256:c9503bbc8821f5c3174e3e7c5ec1683956d8b37e0335c328d26ed29fa402bfc3";
        environment = {
          TZ = " Europe/Zurich ";
          PUID = "1000";
          PGID = "1000";
          VERSION = "docker";
        };
        environmentFiles = [ config.age.secrets.plexClaim.path ];
        volumes = [
          "/var/lib/plex/config:/config"
          "/var/lib/plex/tmp:/transcode"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          ''--mount=type=volume,source=media,target=/mnt/media,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/media,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
          "--network=host"
          "--log-opt=tag='plex'"
        ];
      };
    };
  };
}

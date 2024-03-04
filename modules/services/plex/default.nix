{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.services.az-plex;
  volumePath = "/mnt/media";
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
        image = "lscr.io/linuxserver/plex:1.40.0@sha256:3a948594d80dc6594671a60daf058cd872700e0ddf408958c06d8f3a2f204fb8";
        environment = {
          TZ = " Europe/Zurich ";
          PUID = "1000";
          PGID = "1000";
          VERSION = "docker";
        };
        environmentFiles = [ config.age.secrets.plexClaim.path ];
        volumes = [
          "${volumePath}:/mnt/media"
          "/var/lib/plex/config:/config"
          "/var/lib/plex/tmp:/transcode"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          "--network=host"
          "--log-opt=tag='plex'"
        ];
      };
    };
  };
}

{
  config,
  inputs,
  ...
}:
let
  volumePath = "/mnt/media";
in
{
  imports = [
    "${inputs.self}/modules/services/docker"
  ];
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

  virtualisation.oci-containers = {
    backend = "docker";
    containers."plex" = {
      autoStart = true;
      # https://fleet.linuxserver.io/image?name=linuxserver/plex
      image = "lscr.io/linuxserver/plex:1.42.2@sha256:06f1ee577e910c8cd2dbd9a9bcca82750d1678f994f6d754e55bc3d5f0d699cb";
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
}

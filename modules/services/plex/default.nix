{
  config,
  inputs,
  ...
}:
let
  volumePath = "/mnt/fileserver/media";
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
      image = "lscr.io/linuxserver/plex:1.43.0@sha256:937a04f1cfc72fe6580970b1965325310d7e61d3d97ea3d2adb269e70d32e10b";
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

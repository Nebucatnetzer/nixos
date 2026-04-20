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
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers."plex" = {
      autoStart = true;
      # https://fleet.linuxserver.io/image?name=linuxserver/plex
      image = "lscr.io/linuxserver/plex:1.43.1@sha256:e5c7c283b242966416a4bed2d666acf6f3fb8f957c704be8333f8dc987364825";
      environment = {
        NVIDIA_VISIBLE_DEVICES = "all";
        PGID = "1000";
        PUID = "1000";
        TZ = " Europe/Zurich ";
        VERSION = "docker";
      };
      environmentFiles = [ config.age.secrets.plexClaim.path ];
      ports = [
        "32400:32400"
      ];
      volumes = [
        "${volumePath}:/mnt/media"
        "/var/lib/plex/config:/config"
        "/var/lib/plex/tmp:/transcode"
        "/etc/localtime:/etc/localtime:ro"
      ];
      devices = [ "/dev/dri:/dev/dri" ];
      extraOptions = [
        "--log-opt=tag='plex'"
      ];
    };
  };
}

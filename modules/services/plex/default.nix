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
      image = "lscr.io/linuxserver/plex:1.43.2@sha256:626eb0d52b4522db3be91110a89161be6ccffc0741e9abf0e93aaf4fea98ccb9";
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

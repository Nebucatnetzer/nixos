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
      image = "lscr.io/linuxserver/plex:1.43.0@sha256:937a04f1cfc72fe6580970b1965325310d7e61d3d97ea3d2adb269e70d32e10b";
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

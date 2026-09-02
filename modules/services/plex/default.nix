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
      image = "lscr.io/linuxserver/plex:1.43.3@sha256:7f9a1d574958fc2f177c14ca190d4b811a58c274477f5bae8fb44ee676fb96bf";
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

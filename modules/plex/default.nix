{ custom }: { config, ... }:
{
  age.secrets.plexClaim.file = "${custom.inputs.self}/scrts/plex_claim.age";
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
      image = "lscr.io/linuxserver/plex:1.31.0";
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
}

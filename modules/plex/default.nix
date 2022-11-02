{ inputs, config ? { }, ... }:
{
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
      image = "plexinc/pms-docker";
      autoStart = true;
      environment = {
        TZ = "Europe/Zurich";
      };
      environmentFiles = [ config.age.secrets.plexClaim.path ];
      volumes = [
        "/home/andreas/docker_systems/plex/config:/config"
        "/home/andreas/docker_systems/plex/tmp:/transcode"
      ];
      extraOptions = [
        ''--mount=type=volume,source=media,target=/mnt/media,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/media,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        "--network=host"
      ];
    };
  };
}

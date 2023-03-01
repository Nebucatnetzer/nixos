{ ... }:
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."grav" = {
      image = "lscr.io/linuxserver/grav:1.7.39";
      autoStart = true;
      environment = {
        TZ = "Europe/Zurich";
        PUID = "100";
        PGID = "101";
      };
      ports = [
        "8080:80"
      ];
      volumes = [
        "/etc/timezone:/etc/timezone:ro"
        "/etc/localtime:/etc/localtime:ro"
      ];
      extraOptions = [
        ''--mount=type=volume,source=grav,target=/config,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/grav,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
      ];
    };
  };
}


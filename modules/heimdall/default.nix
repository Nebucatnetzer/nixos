{ ... }:
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."heimdall" = {
      image = "linuxserver/heimdall";
      autoStart = true;
      environment = {
        TZ = "Europe/Zurich";
        PUID = "1000";
        PGID = "100";
      };
      ports = [
        "8081:80"
      ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
      ];
      extraOptions = [
        ''--mount=type=volume,source=heimdall,target=/config,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/heimdall,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
      ];
    };
  };
}


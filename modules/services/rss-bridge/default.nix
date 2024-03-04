{ config, lib, ... }:
let
  cfg = config.services.az-rss-bridge;
  whitelist = builtins.toFile "whitelist.txt" "*";
in
{
  options = {
    services.az-rss-bridge.enable = lib.mkEnableOption "Enable RSS bridge.";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;

    virtualisation.oci-containers = {
      backend = "docker";
      containers."rss-bridge" = {
        # https://hub.docker.com/r/rssbridge/rss-bridge/tags
        image = "rssbridge/rss-bridge@sha256:ac24bd23928f12b8c10c52eca3713276c9015bab82fd6a7fdfe4d9fc05105a58";
        autoStart = true;
        ports = [ "8082:80" ];
        volumes = [
          "${whitelist}:/app/whitelist.txt"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [ "--log-opt=tag='rss-brige'" ];
      };
    };
  };
}

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
        image = "rssbridge/rss-bridge@sha256:00dd67a5dd567af61a8a3013ebd7917abf47b61c2d8581542b4aa650617e3f21";
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

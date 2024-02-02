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
        image = "rssbridge/rss-bridge@sha256:908ee2936c1acdddbff69020cf4f01f259b4e230fd924876b3c8dfbaa8e3326c";
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

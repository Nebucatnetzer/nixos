{ config, lib, ... }:
let
  cfg = config.services.az-rss-bridge;
  whitelist = builtins.toFile "whitelist.txt" ''*'';
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
        image = "rssbridge/rss-bridge@sha256:31d2e27f40b10291f0f2091cb3942fbb0754313dc57fe7c836bccbcab9c893ea";
        autoStart = true;
        ports = [
          "8082:80"
        ];
        volumes = [
          "${whitelist}:/app/whitelist.txt"
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [
          "--log-opt=tag='rss-brige'"
        ];
      };
    };
  };
}

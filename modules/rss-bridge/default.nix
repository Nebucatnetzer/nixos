{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-bridge" = {
      # https://hub.docker.com/r/rssbridge/rss-bridge/tags
      image = "rssbridge/rss-bridge@sha256:d775b232fc0a400aa2c39137a889be93cda8a024bfd2cc865904dd079b40057a";
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
}

{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-bridge" = {
      # https://hub.docker.com/r/rssbridge/rss-bridge/tags
      image = "rssbridge/rss-bridge@sha256:07da0109c0f28c201fdd9e3e988e8029c0324202d33449b3d89c6c215fc279ad";
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

{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-bridge" = {
      # https://hub.docker.com/r/rssbridge/rss-bridge/tags
      image = "rssbridge/rss-bridge@sha256:a3cfc7ec4081ddd3b6666a90bb9d1e9db5cabd1975d1c70b0c90e34b639f72d0";
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

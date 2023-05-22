{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-bridge" = {
      # https://hub.docker.com/r/rssbridge/rss-bridge/tags
      image = "rssbridge/rss-bridge@sha256:7b8be6079488926287d7ba4043d24b679218ccb7bf87b4e47ff0571ec500c345";
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

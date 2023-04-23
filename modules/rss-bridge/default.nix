{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-bridge" = {
      # https://hub.docker.com/r/rssbridge/rss-bridge/tags
      image = "rssbridge/rss-bridge@sha256:21ba82678caf5e0d1d96dc7b7242c012653407d5a9250984ae7e95deefba60c3";
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

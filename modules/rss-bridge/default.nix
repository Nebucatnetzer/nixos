{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-bridge" = {
      image = "rssbridge/rss-bridge@sha256:7683bbdb62a97cf252f66ddc6df0fcf5250052a232e150086e8778c937853320";
      autoStart = true;
      ports = [
        "8082:80"
      ];
      volumes = [
        "${whitelist}:/app/whitelist.txt"
        "/etc/localtime:/etc/localtime:ro"
      ];
    };
  };
}

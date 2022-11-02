{ inputs, ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
  imports = [
    "${inputs.self}/modules/docker"
  ];
  virtualisation.oci-containers = {
    backend = "docker";
    containers."rss-brige" = {
      image = "rssbridge/rss-bridge:latest";
      autoStart = true;
      ports = [
        "8082:80"
      ];
      volumes = [
        "${whitelist}:/app/whitelist.txt"
      ];
    };
  };
}

{ ... }:
let
  whitelist = builtins.toFile "whitelist.txt" ''*'';
in
{
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
  services.nginx = {
    virtualHosts = {
      "rss-bridge.2li.ch" = {
        enableACME = true;
        forceSSL = true;
        listen = [{ port = 4433; addr = "127.0.0.1"; ssl = true; }];
        locations."/" = {
          proxyPass = "http://127.0.0.1:8082";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
      };
    };
  };
}

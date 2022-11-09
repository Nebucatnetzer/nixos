{ custom, hostname }: { pkgs, ... }:
let
  domain = "ttrss.2li.ch";
in
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.115";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/nginx-proxy" { inherit custom domain; })
    "${custom.inputs.self}/modules/rss-bridge"
    (import "${custom.inputs.self}/modules/ttrss-postgres" { inherit custom domain; })
  ];
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

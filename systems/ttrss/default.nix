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
    (import "${custom.inputs.self}/modules/nginx-proxy" {
      domain = "rss-bridge.2li.ch";
      port = "8082";
      inherit custom;
    })
    (import "${custom.inputs.self}/modules/restic-server-client" {
      path = "/var/lib/ttrss";
      tag = "tt-rss";
      time = "23:00";
      inherit custom;
    })
    "${custom.inputs.self}/modules/rss-bridge"
    (import "${custom.inputs.self}/modules/ttrss-postgres" { inherit custom domain; })
  ];
}

{ hostname }: { inputs, pkgs, ... }:
let
  domain = "ttrss.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.115";
      inherit hostname;
    })
    "${inputs.self}/modules/docker"
    (import "${inputs.self}/modules/nginx-proxy" {
      domain = "rss-bridge.2li.ch";
      port = "8082";
    })
    (import "${inputs.self}/modules/restic-client-server-postgres" {
      path = "/var/lib/ttrss";
      tag = "tt-rss";
      time = "23:00";
    })
    "${inputs.self}/modules/rss-bridge"
    (import "${inputs.self}/modules/ttrss-postgres" { inherit domain; })
  ];
}

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
    (import "${inputs.self}/modules/ttrss-postgres" { inherit domain; })
  ];
  services = {
    az-nginx-proxy = {
      enable = true;
      domain = "rss-bridge.2li.ch";
      port = 8082;
    };
    az-restic-client-server-postgres = {
      enable = true;
      path = "/var/lib/ttrss";
      tag = "tt-rss";
      time = "23:00";
    };
    az-rss-bridge.enable = true;
  };
}

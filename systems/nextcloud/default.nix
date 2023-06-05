{ hostname }: { inputs, pkgs, ... }:
let
  domain = "nextcloud.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.103";
      inherit hostname;
    })
    (import "${inputs.self}/modules/nextcloud" {
      inherit domain;
    })
    "${inputs.self}/modules/nginx-acme-base"
    (import "${inputs.self}/modules/restic-client-server-mysql" {
      path = "/home/andreas";
      time = "01:30";
    })
  ];

  services = {
    az-mariadb-for-containers.enable = true;
    nginx = {
      appendHttpConfig = ''
        # Allow embedding from same domain
        add_header X-Frame-Options SAMEORIGIN;
      '';
      clientMaxBodySize = "20G";
      virtualHosts."${domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8080";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
        extraConfig = ''
          # Required for large downloads
          proxy_buffering off;
        '';
      };
    };
  };
}

{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.103";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/restic-server-mysql-client" {
      path = "/home/andreas";
      time = "04:00"; inherit custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/mariadb"
    "${custom.inputs.self}/modules/nginx-acme-base"
  ];

  services.nginx = {
    appendHttpConfig = ''
      # Allow embedding from same domain
      add_header X-Frame-Options SAMEORIGIN;
    '';
    clientMaxBodySize = "20G";
    virtualHosts."nextcloud.2li.ch" = {
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
}

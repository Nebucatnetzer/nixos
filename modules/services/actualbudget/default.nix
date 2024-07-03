{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-actualbudget;
in
{
  options = {
    services.az-actualbudget.enable = lib.mkEnableOption "Enable Actualbudget";
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;
      virtualHosts."actualbudget.zweili.org" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:5006";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
      };
    };
    virtualisation.oci-containers = {
      backend = "docker";
      containers."actualbudget" = {
        # https://hub.docker.com/r/mailserver/docker-mailserver/tags
        image = "ghcr.io/actualbudget/actual-server:24.7.0@sha256:12cb8d9b8e653b66e185360e980c08e90f8c1f40bf4fb34dcb90eb270329d996";
        autoStart = true;
        ports = [ "5006:5006" ];
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
          "/var/lib/actualbudget:/data"
        ];
        extraOptions = [ "--log-opt=tag='actualbudget'" ];
      };
    };
  };
}

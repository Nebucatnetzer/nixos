{
  config,
  lib,
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
      virtualHosts."actual.zweili.org" = {
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
        image = "ghcr.io/actualbudget/actual-server:25.8.0@sha256:3d7fb08d3b405426e8625013a5ed95f5015e6c04d0f044e2e3fe7d7b7a41951b";
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

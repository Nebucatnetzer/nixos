{ config, lib, ... }:
let
  cfg = config.services.az-search;
in
{
  options = {
    services.az-search.enable = lib.mkEnableOption "Enable Search";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;
    virtualisation.oci-containers = {
      backend = "docker";
      containers."zweili-search" = {
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:06a9d3c67a7c95044b06c5fe9aebc83bf95bc9695724750c1e6fe55454d39828";
        autoStart = true;
        environment = {
          ZWEILI_SEARCH_DOMAIN = "search.zweili.org";
          DEBUG = "true";
        };
        ports = [ "8080:8000" ];
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
          "/var/lib/zweili_search:/var/lib/zweili_search"
        ];
        extraOptions = [ "--log-opt=tag='zweili-search'" ];
      };
    };
  };
}

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
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:a30477f24a95c51b433721255cf21317692f378d3e2f5789d023d38c6f22ed67";
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

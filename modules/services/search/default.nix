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
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:388be9eb1dbccba09841c275fa31557f5d5e06b28826f0f4dc0f7758f185868f";
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

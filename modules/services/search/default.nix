{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-search;
  networkName = "zweili-search";
in
{
  options = {
    services.az-search.enable = lib.mkEnableOption "Enable Search";
  };

  config = lib.mkIf cfg.enable {
    services.az-docker.enable = true;
    virtualisation.oci-containers = {
      backend = "docker";
      containers."zweili-search-app" = {
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:a7554df9ae5a152f323a973b3c61677b7d4ea2d9cead96ab6a4594890e48e30a";
        autoStart = true;
        environment = {
          ZWEILI_SEARCH_DOMAIN = "search.zweili.org";
        };
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
          "/var/lib/zweili_search:/var/lib/zweili_search"
        ];
        extraOptions = [ "--log-opt=tag='zweili-search-app'" ];
        networks = [ networkName ];
      };
      containers."zweili-search-nginx" = {
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-nginx:latest@sha256:d43b68089aa9ddd2c9c2e6be186920dcdbde3ab1bc3b4e50108cd8481d3baf1e";
        autoStart = true;
        ports = [ "8080:80" ];
        volumes = [
          "/etc/localtime:/etc/localtime:ro"
        ];
        extraOptions = [ "--log-opt=tag='zweili-search-nginx'" ];
        networks = [ networkName ];
      };
    };
    systemd.services."docker-network-${networkName}" = {
      path = [ pkgs.docker ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStop = "docker network rm -f ${networkName}";
      };
      script = ''
        docker network inspect ${networkName} || docker network create ${networkName}
      '';
    };
  };
}

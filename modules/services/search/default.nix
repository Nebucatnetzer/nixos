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
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:a688f167e4e918c77af4d25ec3a3712deecb327ce7972543c824cba77bc42ad1";
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
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-nginx:latest@sha256:d61455777baa095412de80d110c7653fb457db58c602d3b649506656905ff7a8";
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

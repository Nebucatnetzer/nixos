{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-search;
  networkName = "zweili-search";
  searxngEnv = config.age.secrets.searxngEnv.path;
in
{
  options = {
    services.az-search.enable = lib.mkEnableOption "Enable Search";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.searxngEnv = {
      file = "${inputs.self}/scrts/searxng_env.age";
      mode = "640";
      owner = "root";
      group = config.systemd.services.searx.serviceConfig.Group;
    };
    age.secrets.zweiliSearchEnv.file = "${inputs.self}/scrts/zweili_search_env.age";
    services.az-docker.enable = true;
    virtualisation.oci-containers = {
      backend = "docker";
      containers."zweili-search-app" = {
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:41315364feb66c96ac5e06ece628bbce1e5675e7c1fa7192dc435f203f96769a";
        autoStart = true;
        environmentFiles = [ config.age.secrets.zweiliSearchEnv.path ];
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
        image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-nginx:latest@sha256:7991f933fd4ff5b94ccdee2abedfd0c45a2e2bdb8a0445fcf77e22f8e223fe0e";
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

    services = {
      searx = {
        enable = true;
        environmentFile = searxngEnv;
        faviconsSettings = {
          favicons = {
            cfg_schema = 1;
            cache = {
              db_url = "/run/searx/faviconcache.db";
              HOLD_TIME = 5184000;
              LIMIT_TOTAL_BYTES = 2147483648;
              BLOB_MAX_BYTES = 40960;
              MAINTENANCE_MODE = "auto";
              MAINTENANCE_PERIOD = 600;
            };
          };
        };
        settings = {
          use_default_settings = true;
          search = {
            autocomplete = "duckduckgo";
            favicon_resolver = "duckduckgo";
            default_lang = "en";
            languages = [
              "de-CH"
              "en"
            ];
          };
          server = {
            base_url = "https://searxng.zweili.org";
            bind_address = "0.0.0.0";
            image_proxy = true;
            port = 8081;
            public_instance = false;

            # Plugin configs
            plugins = {
              "searx.plugins.calculator.SXNGPlugin" = {
                active = true;
              };
              "searx.plugins.hash_plugin.SXNGPlugin" = {
                active = true;
              };
              "searx.plugins.self_info.SXNGPlugin" = {
                active = true;
              };
              "searx.plugins.tracker_url_remover.SXNGPlugin" = {
                active = true;
              };
              "searx.plugins.unit_converter.SXNGPlugin" = {
                active = true;
              };
              "searx.plugins.hostnames.SXNGPlugin" = {
                active = true;
              };
            };

            hostnames = {
              high_priority = [ "wiki.nixos.org" ];
              remove = [ "nixos.wiki" ];
            };
          };
        };
      };
    };
  };
}

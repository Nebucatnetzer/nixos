{
  config,
  inputs,
  pkgs,
  ...
}:
let
  networkName = "zweili-search";
  searxngEnv = config.age.secrets.searxngEnv.path;
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in
{
  imports = [
    "${inputs.self}/modules/services/docker"
  ];
  age.secrets.searxngEnv = {
    file = "${inputs.self}/scrts/searxng_env.age";
    mode = "640";
    owner = "root";
    group = config.systemd.services.searx.serviceConfig.Group;
  };
  age.secrets.zweiliSearchEnv.file = "${inputs.self}/scrts/zweili_search_env.age";
  virtualisation.oci-containers = {
    backend = "docker";
    containers."zweili-search-app" = {
      image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:908c6c5ee8ed060fa7f800e1ec50812c36dd96e98e795cd22a284d30b7721d87";
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
      image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-nginx:latest@sha256:b22175ff669b6aacb42865593f63ab8fd9fa276a9cd34740d5b89d1991a66765";
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
      package = unstable.searxng;
      # https://github.com/searxng/searxng/blob/master/searx/settings.yml
      settings = {
        use_default_settings = true;
        engines = [
          {
            name = "yandex";
            engine = "yandex";
            categories = "general";
            search_type = "web";
            shortcut = "yd";
            disabled = true;
            inactive = false;
          }
          {
            name = "yandex images";
            engine = "yandex";
            categories = "images";
            search_type = "images";
            shortcut = "ydi";
            disabled = true;
            inactive = false;
          }
        ];
        search = {
          autocomplete = "google";
          default_lang = "en";
          favicon_resolver = "duckduckgo";
          languages = [
            "de-CH"
            "en"
          ];
        };
        server = {
          base_url = "https://searxng.zweili.org";
          bind_address = "0.0.0.0";
          image_proxy = true;
          method = "GET";
          port = 8081;
          public_instance = false;
        };
        ui = {
          hotkeys = "vim";
          query_in_title = true;
          results_on_new_tab = false;
          url_formatting = "full";
        };

        # Plugin configs
        hostnames = {
          high_priority = [
            ''(.*\.)?wiki.nixos.org''
            ''(.*\.)?nix.dev''
            ''(.*\.)?reddit.com''
            ''(.*\.)?wikipedia.org$''
          ];
          low_priority = [
            ''(.*\.)?medium.com''
          ];
          remove = [
            ''search.nixos.org''
            ''(.*\.)?nixos.wiki''
          ];
        };
      };
    };
  };
}

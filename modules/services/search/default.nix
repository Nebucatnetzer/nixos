{
  config,
  inputs,
  pkgs,
  ...
}:
let
  searxngHtpasswd = config.age.secrets.searxngHtpasswd.path;
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
  age.secrets.searxngHtpasswd = {
    file = "${inputs.self}/scrts/searxng_htpasswd.age";
    mode = "640";
    owner = "root";
    group = config.services.nginx.group;
  };
  age.secrets.zweiliSearchEnv.file = "${inputs.self}/scrts/zweili_search_env.age";
  virtualisation.oci-containers = {
    backend = "docker";
    containers."zweili-search-app" = {
      image = "ghcr.io/nebucatnetzer/meta-search/zweili-search-app:latest@sha256:3710fa8692ffb42d8ab3922d2b4c18bec6ee292ea4e5da054bbe8e1ebf6c734e";
      autoStart = true;
      environmentFiles = [ config.age.secrets.zweiliSearchEnv.path ];
      environment = {
        ZWEILI_SEARCH_DOMAIN = "search.zweili.org";
      };
      ports = [ "8000:8000" ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "/var/lib/zweili_search:/var/lib/zweili_search"
      ];
      extraOptions = [ "--log-opt=tag='zweili-search-app'" ];
    };
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts = {
        "search.zweili.org" = {
          enableACME = true;
          forceSSL = true;
          listen = [
            {
              port = 8433;
              addr = "127.0.0.1";
              ssl = true;
            }
          ];
          locations."/" = {
            proxyPass = "http://127.0.0.1:8000";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
        };
        "searxng.zweili.org" = {
          enableACME = true;
          forceSSL = true;
          listen = [
            {
              port = 8433;
              addr = "127.0.0.1";
              ssl = true;
            }
          ];
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            if ($http_user_agent ~* "Bytespider|PetalBot|ClaudeBot|YandexBot|meta-externalagent|Amazonbot|Crawlers|facebookexternalhit|ImagesiftBot|Barkrowler|Googlebot|bingbot") { return 403; }
          '';
          locations."/search" = {
            basicAuthFile = searxngHtpasswd;
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          locations."/stats" = {
            basicAuthFile = searxngHtpasswd;
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
        };
      };
    };
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
          bind_address = "127.0.0.1";
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
            "search.nixos.org"
            ''(.*\.)?nixos.wiki''
          ];
        };
      };
    };
  };
}

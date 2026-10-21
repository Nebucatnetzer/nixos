{
  config,
  inputs,
  unstable-pkgs,
  ...
}:
let
  botBlockRegex = import "${inputs.self}/modules/services/nginx/lib/nginx-bot-block.nix";
  denyPrivateNetworks = import "${inputs.self}/modules/services/lib/deny-private-networks.nix";
  searxngHtpasswd = config.age.secrets.searxngHtpasswd.path;
  searxngEnv = config.age.secrets.searxngEnv.path;
in
{
  imports = [
    inputs.meta-search.nixosModules.default
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
    inherit (config.services.nginx) group;
  };
  age.secrets.zweiliSearchEnv.file = "${inputs.self}/scrts/zweili_search_env.age";
  systemd.services.searx.serviceConfig = denyPrivateNetworks;
  systemd.services.zweili-search.serviceConfig = denyPrivateNetworks;
  systemd.services.zweili-search-setup.serviceConfig = denyPrivateNetworks;
  services = {
    zweili-search = {
      enable = true;
      domain = "search.zweili.org";
      environmentFile = config.age.secrets.zweiliSearchEnv.path;
    };
    nginx = {
      enable = true;
      virtualHosts = {
        "search.zweili.org" = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8000";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          extraConfig = ''
            if ($http_user_agent ~* "${botBlockRegex}") { return 403; }
          '';
        };
        "searxng.zweili.org" = {
          enableACME = true;
          forceSSL = true;
          basicAuthFile = searxngHtpasswd;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            proxyWebsockets = true; # needed if you need to use WebSocket
          };
          # Browsers fetch this without credentials when they add the search engine.
          locations."= /opensearch.xml" = {
            proxyPass = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
            extraConfig = ''
              auth_basic off;
            '';
          };
          extraConfig = ''
            if ($http_user_agent ~* "${botBlockRegex}") { return 403; }
          '';
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
      package = unstable-pkgs.searxng;
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

{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  system = pkgs.system;
  cfg = config.services.az-attic-server;
  cacheStorage = "/mnt/binary-cache";
  atticPort = 8080;
  atticDomain = "cache.zweili.org";
  atticCollectGarbage = pkgs.writeShellScriptBin "attic-collect-garbage" ''
    ATTICD=${inputs.attic.packages.${system}.attic-server}/bin/atticd

    exec ${pkgs.systemd}/bin/systemd-run \
      --quiet \
      --pty \
      --same-dir \
      --wait \
      --collect \
      --service-type=exec \
      --property=EnvironmentFile=${config.services.atticd.credentialsFile} \
      --property=DynamicUser=yes \
      --property=User=${config.services.atticd.user} \
      --property=Environment=ATTICADM_PWD=$(pwd) \
      --property=ReadWritePaths=${config.services.atticd.settings.storage.path} \
      --working-directory / \
      -- \
      $ATTICD \
      --config ${config.services.atticd.configFile} \
      --mode garbage-collector-once
  '';
in
{
  imports = [ inputs.attic.nixosModules.atticd ];
  options = {
    services.az-attic-server.enable = lib.mkEnableOption "Enable attic server and related services";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.atticEnv = {
      file = "${inputs.self}/scrts/attic_env.age";
      mode = "600";
      owner = "65312";
      group = "65312";
    };
    fileSystems."${cacheStorage}" = {
      device = "10.7.89.108:binary-cache";
      fsType = "nfs";
      options = [
        "hard"
        "noatime"
        "rw"
      ];
    };
    environment.systemPackages = [
      inputs.attic.packages.${system}.attic-client
      atticCollectGarbage
    ];

    networking.firewall.allowedTCPPorts = [ 443 ];

    services.az-nginx-proxy = {
      enable = true;
      domain = atticDomain;
      port = atticPort;
    };
    services.nginx = {
      clientMaxBodySize = "1G";
      proxyTimeout = "300s";
    };
    services.atticd = {
      enable = true;
      credentialsFile = config.age.secrets.atticEnv.path;
      user = "atticd";
      group = "atticd";
      settings = {
        listen = "[::]:${toString atticPort}";
        api-endpoint = "https://${atticDomain}/";
        allowed-hosts = [ atticDomain ];
        storage = {
          type = "local";
          path = "${cacheStorage}";
        };
        chunking = {
          nar-size-threshold = 64 * 1024; # 64 KiB
          # The preferred minimum size of a chunk, in bytes
          min-size = 16 * 1024; # 16 KiB
          # The preferred average size of a chunk, in bytes
          avg-size = 64 * 1024; # 64 KiB
          # The preferred maximum size of a chunk, in bytes
          max-size = 256 * 1024; # 256 KiB
        };
        database.url = "postgresql:///${config.services.atticd.user}?host=/run/postgresql";
        garbage-collection = {
          interval = "0h";
          default-retention-period = "6 months";
        };
      };
    };
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      ensureUsers = [
        {
          name = "${config.services.atticd.user}";
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [ "${config.services.atticd.user}" ];
    };
  };
}

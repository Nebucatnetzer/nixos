{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-attic-server;
  cacheStorage = "/mnt/binary-cache";
  atticPort = 8080;
in
{
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
    environment.systemPackages = [ pkgs.unstable.attic-client ];

    networking.firewall.allowedTCPPorts = [ atticPort ];
    services.atticd = {
      enable = true;
      credentialsFile = config.age.secrets.atticEnv.path;
      settings = {
        listen = "[::]:${toString atticPort}";
        api-endpoint = "http://management.2li.local:${toString atticPort}/";
        allowed-hosts = [ ];
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
        database.url = "postgresql:///atticd?host=/run/postgresql";
        garbage-collection = {
          interval = "24h";
          default-retention-period = "6 months";
        };
      };
    };
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      ensureUsers = [
        {
          name = "atticd";
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [ "atticd" ];
    };
  };
}

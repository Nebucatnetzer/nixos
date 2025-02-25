{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-librenms;
in
{
  options = {
    services.az-librenms.enable = lib.mkEnableOption "Enable the LibreNMS service.";
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 ];
    services = {
      az-snmpd.enable = true;
      librenms = {
        enable = true;
        database = {
          createLocally = true;
          socket = "/run/mysqld/mysqld.sock";
        };
        settings = {
          enable_syslog = true;
          ignore_mount_string = [
            "store"
            "run"
            "server-data"
            "shm"
          ];
        };
      };
    };
  };
}

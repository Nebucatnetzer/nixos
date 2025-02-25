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
        extraConfig = ''
          $config['ignore_mount_string'][] = "store";
          $config['ignore_mount_string'][] = "run";
          $config['ignore_mount_string'][] = "server-data";
          $config['ignore_mount_string'][] = "shm";
        '';
      };
    };
  };
}

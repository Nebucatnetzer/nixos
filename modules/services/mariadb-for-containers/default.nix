{ config, lib, pkgs, ... }:
let
  cfg = config.services.az-mariadb-for-containers;
in
{
  options = {
    services.az-mariadb-for-containers.enable = lib.mkEnableOption "Enable MariaDB configured for container clients.";
  };

  config = lib.mkIf cfg.enable {
    services.mysql = {
      enable = true;
      package = pkgs.mariadb_106;
      settings = {
        mysqld = {
          bind-address = "172.17.0.1";
        };
      };
    };
    networking.firewall.extraCommands = "iptables -A INPUT -p tcp --destination-port 3306 -s 172.16.0.0/12 -j ACCEPT";
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-postgresql;
in
{
  options = {
    services.az-postgresql.enable = lib.mkEnableOption "Enable PostgreSQL with settings for container clients.";
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      enableTCPIP = true;
      package = pkgs.postgresql_14;
      settings.listen_addresses = pkgs.lib.mkForce "127.0.0.1,172.17.0.1";
    };
    networking.firewall.extraCommands = "iptables -A INPUT -p tcp --destination-port 5432 -s 172.16.0.0/12 -j ACCEPT";
  };
}

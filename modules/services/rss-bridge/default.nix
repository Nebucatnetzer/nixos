{ config, lib, ... }:
let
  cfg = config.services.az-rss-bridge;
in
{
  options = {
    services.az-rss-bridge.enable = lib.mkEnableOption "Enable RSS bridge.";
    services.az-rss-bridge.domain = lib.mkOption {
      type = with lib.types; str;
      default = "rss-bridge";
      description = "Domain to use for the RSS bridge.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.az-acme-base = {
      enable = true;
    };
    services.nginx.virtualHosts."${cfg.domain}" = {
      enableACME = true;
      forceSSL = true;
    };
    services.rss-bridge = {
      enable = true;
      config.system.enabled_bridges = [ "*" ];
      virtualHost = cfg.domain;
    };
  };
}

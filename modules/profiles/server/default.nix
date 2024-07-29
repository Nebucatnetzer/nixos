{ config, lib, ... }:
let
  cfg = config.profiles.az-server;
in
{
  options = {
    profiles.az-server.enable = lib.mkEnableOption "Enable server profile";
  };
  config = lib.mkIf cfg.enable { services.az-syslog.enable = true; };
}

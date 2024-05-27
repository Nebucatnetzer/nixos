{ config, lib, ... }:
let
  cfg = config.profiles.az-server;
in
{
  options = {
    profiles.az-server.enable = lib.mkEnableOption "Enable server profile";
  };
  config = lib.mkIf cfg.enable {
    services.az-syslog.enable = true;
    system.autoUpgrade = {
      enable = true;
      dates = "03:00";
      flake = lib.mkDefault "git+https://git.2li.ch/Nebucatnetzer/nixos#${config.networking.hostName}";
    };
  };
}

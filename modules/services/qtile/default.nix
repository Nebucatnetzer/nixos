{ config, lib, pkgs, ... }:
let
  cfg = config.services.az-qtile;
in
{
  options = {
    services.az-qtile.enable = lib.mkEnableOption "Enable Qtile window manager.";
  };

  config = lib.mkIf cfg.enable {
    services = {
      xserver = {
        displayManager.defaultSession = "none+qtile";
        windowManager.qtile.enable = true;
      };
    };
    home-manager.users.${config.az-username} = {
      home.file.".config/qtile/config.py".source = ./config.py;
      home.file.".config/qtile/autostart.sh".source = ./autostart.sh;
      home.packages = [
        pkgs.pulseaudio # required for volume controls in qtile
      ];
    };
  };
}

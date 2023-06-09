{ config, inputs, lib, pkgs, ... }:
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
      home.file.".config/qtile/config.py".source = "${inputs.self}/home-manager/configs/qtile/config.py";
      home.file.".config/qtile/autostart.sh".source = "${inputs.self}/home-manager/configs/qtile/autostart.sh";
      home.packages = [
        pkgs.pulseaudio # required for volume controls in qtile
      ];
    };
  };
}

{ config, lib, ... }:
let
  cfg = config.hardware.az-bluetooth;
in
{
  options = {
    hardware.az-bluetooth.enable = lib.mkEnableOption "Enable Bluetooth";
  };

  config = lib.mkIf cfg.enable {
    # Blueooth support in general
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
    # Xbox Controller support
    hardware.xpadneo.enable = true;

    # Blueman applet
    services.blueman.enable = true;

    home-manager.users.${config.az-username} = {
      services.blueman-applet.enable = true;
      systemd.user.services.blueman-applet = {
        Unit.After = [
          "graphical-session.target"
        ];
        Service.Restart = "on-failure";
      };
    };
  };
}

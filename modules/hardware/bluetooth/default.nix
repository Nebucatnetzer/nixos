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

    systemd.user.services.blueman-applet = {
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        Restart = "always";
      };
    };
  };
}

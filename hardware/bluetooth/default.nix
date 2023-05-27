{ config, lib, ... }:
let
  cfg = config.hardware.az_bluetooth;
in
{
  options = {
    hardware.az_bluetooth.enable = lib.mkEnableOption "Enable Bluetooth";
  };

  config = lib.mkIf cfg.enable {
    # Blueooth support in general
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = false;
    };

    # Blueman applet
    services.blueman.enable = true;

    systemd.user.services.blueman-applet = {
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
  };
}


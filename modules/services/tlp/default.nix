{ config, lib, ... }:
let
  cfg = config.services.az-tlp;
in
{
  options = {
    services.az-tlp.enable = lib.mkEnableOption "Enable TLP";
  };

  config = lib.mkIf cfg.enable {
    services.tlp = {
      enable = true;
      settings = {
        DEVICES_TO_DISABLE_ON_LAN_CONNECT = "wifi";
        DEVICES_TO_ENABLE_ON_STARTUP = "bluetooth";
        DEVICES_TO_ENABLE_ON_DOCK = "bluetooth";
        DEVICES_TO_ENABLE_ON_UNDOCK = "wifi";
        DEVICES_TO_DISABLE_ON_UNDOCK = "bluetooth";
      };
    };
  };
}

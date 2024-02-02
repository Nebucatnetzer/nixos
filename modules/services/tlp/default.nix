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
        DEVICES_TO_DISABLE_ON_STARTUP = "bluetooth wifi wwan";
        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth wifi wwan";
      };
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-grobi;
in
{
  options = {
    services.az-grobi.enable = lib.mkEnableOption "Enabel grobi display manager.";
  };

  config = lib.mkIf cfg.enable {
    services.grobi = {
      enable = true;
      rules = [
        {
          name = "virtualbox";
          outputs_connected = [ "Virtual1" ];
          configure_single = "Virtual1@3840x2160";
          primary = true;
          atomic = true;
          execute_after = [
            "${pkgs.nitrogen}/bin/nitrogen --restore"
            "${pkgs.qtile-unwrapped}/bin/qtile cmd-obj -o cmd -f restart"
          ];
        }
        {
          name = "docked";
          outputs_connected = [
            "eDP-1"
            "DP-1-2"
          ];
          atomic = true;
          configure_single = "DP-1-2";
          primary = true;
          execute_after = [
            "${pkgs.nitrogen}/bin/nitrogen --restore"
            "${pkgs.qtile-unwrapped}/bin/qtile cmd-obj -o cmd -f restart"
            "${pkgs.networkmanager}/bin/nmcli radio wifi off"
          ];
        }
        {
          name = "undocked";
          outputs_disconnected = [ "DP-1-2" ];
          configure_single = "eDP-1";
          primary = true;
          atomic = true;
          execute_after = [
            "${pkgs.nitrogen}/bin/nitrogen --restore"
            "${pkgs.qtile-unwrapped}/bin/qtile cmd-obj -o cmd -f restart"
            "${pkgs.networkmanager}/bin/nmcli radio wifi on"
          ];
        }
        {
          name = "fallback";
          configure_single = "eDP-1";
        }
      ];
    };
  };
}

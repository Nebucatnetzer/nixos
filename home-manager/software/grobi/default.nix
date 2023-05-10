{ pkgs, ... }:
{
  services.grobi = {
    enable = true;
    rules = [
      {
        name = "docked";
        outputs_connected = [ "DP-1-1" "DP-1-2" ];
        atomic = true;
        configure_row = [ "DP-1-2" "DP-1-1" ];
        primary = "DP-1-1";
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
          "${pkgs.qtile}/bin/qtile cmd-obj -o cmd -f restart"
        ];
      }
      {
        name = "gaming";
        outputs_connected = [ "DP-1-2" ];
        outputs_disconnected = [ "DP-1-1" ];
        configure_single = "DP-1-2";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
          "${pkgs.qtile}/bin/qtile cmd-obj -o cmd -f restart"
        ];
      }
      {
        name = "undocked";
        outputs_disconnected = [ "DP-1-1" "DP-1-2" ];
        configure_single = "eDP-1";
        primary = true;
        atomic = true;
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
          "${pkgs.qtile}/bin/qtile cmd-obj -o cmd -f restart"
          "${pkgs.networkmanager}/bin/nmcli radio wifi on"
        ];
      }
      {
        name = "fallback";
        configure_single = "eDP-1";
      }
    ];
  };
}

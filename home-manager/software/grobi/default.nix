{ pkgs, ... }:
{
  services.grobi = {
    enable = true;
    rules = [
      {
        name = "docked";
        outputs_connected = [ "eDP-1" "DP-1" ];
        atomic = true;
        configure_row = [ "DP-1" "eDP-1" ];
        primary = "DP-1";
        execute_after = [
          "${pkgs.nitrogen}/bin/nitrogen --restore"
          "${pkgs.qtile}/bin/qtile cmd-obj -o cmd -f restart"
          "${pkgs.networkmanager}/bin/nmcli radio wifi off"
        ];
      }
      {
        name = "undocked";
        outputs_disconnected = [ "DP-1" ];
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

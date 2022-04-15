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
      }
      {
        name = "undocked";
        outputs_disconnected = [ "DP-1-1" "DP-1-2" ];
        configure_single = "eDP-1";
        primary = true;
        atomic = true;
      }
      {
        name = "fallback";
        configure_single = "eDP-1";
      }
    ];
  };
}

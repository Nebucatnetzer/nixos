{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-grobi;
  dp = "DP-1-2";
  hdmi = "HDMI-1";
  notebookScreen = "eDP-1";
in
{
  options = {
    services.az-grobi.enable = lib.mkEnableOption "Enabel grobi display manager.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.grobi ];
    systemd.user.services.grobi = {
      Service = {
        WorkingDirectory = config.home.homeDirectory;
      };
    };
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
            notebookScreen
            dp
          ];
          atomic = true;
          configure_single = dp;
          primary = true;
          execute_after = [
            "${pkgs.nitrogen}/bin/nitrogen --restore"
            "${pkgs.qtile-unwrapped}/bin/qtile cmd-obj -o cmd -f restart"
            "${pkgs.networkmanager}/bin/nmcli radio wifi off"
          ];
        }
        {
          name = "hdmi";
          outputs_connected = [
            hdmi
            notebookScreen
          ];
          atomic = true;
          configure_row = [
            (hdmi + "@3840x2160")
            notebookScreen
          ];
          primary = hdmi;
          execute_after = [
            "${pkgs.nitrogen}/bin/nitrogen --restore"
            "${pkgs.qtile-unwrapped}/bin/qtile cmd-obj -o cmd -f restart"
            "${pkgs.networkmanager}/bin/nmcli radio wifi off"
          ];
        }
        {
          name = "undocked";
          outputs_disconnected = [ dp ];
          configure_single = notebookScreen;
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
          configure_single = notebookScreen;
        }
      ];
    };
  };
}

{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-signal;
  signal = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.signal-desktop;
in
{
  options = {
    programs.az-signal.enable = lib.mkEnableOption "Enable Signal.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ signal ];
    xdg.desktopEntries = {
      signal = {
        name = "Signal with tray icon";
        exec = "${signal}/bin/signal-desktop-bin --use-tray-icon --no-sandbox %U";
        terminal = false;
        type = "Application";
        icon = "signal-desktop";
        comment = "Private messaging from your desktop";
        mimeType = [
          "x-scheme-handler/sgnl"
          "x-scheme-handler/signalcaptcha"
        ];
        categories = [
          "Network"
          "InstantMessaging"
          "Chat"
        ];
      };
    };
  };
}

{ inputs, pkgs, ... }:
let
  signal = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.signal-desktop;
in
{
  home.packages = [ signal ];
  xdg.desktopEntries = {
    signal = {
      name = "Signal with tray icon";
      exec = "${signal}/bin/signal-desktop --use-tray-icon --no-sandbox %U";
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
}

{ pkgs, unstable-pkgs, ... }:
let
  signal = unstable-pkgs.signal-desktop;
  signalTray = pkgs.makeDesktopItem {
    name = "signal";
    desktopName = "Signal with tray icon";
    exec = "${signal}/bin/signal-desktop --use-tray-icon --no-sandbox %U";
    icon = "signal-desktop";
    comment = "Private messaging from your desktop";
    mimeTypes = [
      "x-scheme-handler/sgnl"
      "x-scheme-handler/signalcaptcha"
    ];
    categories = [
      "Network"
      "InstantMessaging"
      "Chat"
    ];
    terminal = false;
    type = "Application";
  };
in
{
  environment.systemPackages = [
    signal
    signalTray
  ];
}

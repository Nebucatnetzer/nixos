{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.signal-desktop
  ];

  xdg.desktopEntries = {
    signal = {
      name = "Signal with tray icon";
      exec = "${pkgs.unstable.signal-desktop}/bin/signal-desktop --use-tray-icon --no-sandbox %U";
      terminal = false;
      type = "Application";
      icon = "signal-desktop";
      comment = "Private messaging from your desktop";
      mimeType = [ "x-scheme-handler/sgnl" "x-scheme-handler/signalcaptcha" ];
      categories = [ "Network" "InstantMessaging" "Chat" ];
    };
  };
}

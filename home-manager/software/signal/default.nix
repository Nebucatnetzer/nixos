{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.signal-desktop
  ];

  home.file.".config/qtile/autostart.d/signal.sh".source = ./signal.sh;
  xdg.desktopEntries = {
    signal = {
      name = "Signal";
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

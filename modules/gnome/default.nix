{ pkgs, ... }: {
  services.switcherooControl.enable = true;
  services.touchegg.enable = true;
  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  services.xserver.desktopManager.gnome.enable = true;
  environment = {
    systemPackages = with pkgs; [
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      gnomeExtensions.gtk-title-bar
      gnomeExtensions.switcheroo
      gnomeExtensions.x11-gestures
    ];
    gnome.excludePackages = (with pkgs; [
      gnome-console
      gnome-text-editor
      gnome-tour
    ]) ++ (with pkgs.gnome; [
      atomix # puzzle game
      cheese # webcam tool
      epiphany # web browser
      geary
      gedit # text editor
      gnome-characters
      gnome-clocks
      gnome-logs
      gnome-maps
      gnome-music
      gnome-weather
      hitori # sudoku game
      iagno # go game
      tali # poker game
      totem # video player
      yelp
    ]);
  };
}

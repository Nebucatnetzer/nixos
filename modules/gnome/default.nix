{ custom }: { pkgs, ... }:
{
  services.switcherooControl.enable = true;
  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  services.xserver.desktopManager.gnome.enable = true;
  environment = {
    systemPackages = with pkgs; [
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      gnomeExtensions.gtk-title-bar
      gnomeExtensions.switcheroo
    ];
    gnome.excludePackages = (with pkgs; [
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
  services.touchegg.enable = true;
  home-manager.users.${custom.username} = {
    home.file.".config/touchegg/touchegg.conf".source = ./touchegg.conf;
  };

  # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
  # If no user is logged in, the machine will power down after 20 minutes.
  systemd.targets = {
    sleep.enable = false;
    suspend.enable = false;
    hibernate.enable = false;
    hybrid-sleep.enable = false;
  };
}

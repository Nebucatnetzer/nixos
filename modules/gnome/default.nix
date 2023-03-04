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
  services.touchegg.enable = true;
  home-manager.users.${custom.username} = {
    home.file.".config/touchegg/touchegg.conf".source = ./touchegg.conf;
    dconf.settings = {
      "/org/gnome/desktop" = {
        input-sources = {
          xkb-options = [ "compose:ralt" ];
        };
        "wm/keybindings" = {
          show-desktop = [ "<Super> d" ];
          switch-applications = [ "<Super>Tab" ];
          witch-windows = [ "<Alt>Tab" ];
        };
        interface = {
          color-scheme = "default";
        };
        privacy = {
          recent-files-max-age = 30;
          remember-recent-files = true;
          remove-old-trash-files = true;
          remove-old-temp-files = true;
          old-files-age = "uint32 30";
        };
        screensaver = {
          lock-delay = "uint32 0";
          lock-enabled = true;
        };
        "session/idle-delay" = "uint32 300";
      };
      "/org/gnome/settings-daemon/plugins/media-keys" = {
        home = [ "<Super>e" ];
        www = [ "<Super>w" ];
        volume-mute = [ "<Super>F1" ];
        volume-down = [ "<Super>F2" ];
        volume-up = [ "<Super>F3" ];
        play = [ "<Super>F6" ];
        previous = [ "<Super>F5" ];
        next = [ "<Super>F7" ];
      };
      "/org/freedesktop/tracker/miner/files/index-recursive-directories" = [
        "&DOCUMENTS"
        "&MUSIC"
        "&PICTURES"
        "&VIDEOS"
        "/home/andreas/nextcloud/10_documents"
        "&DOWNLOAD"
      ];
      "/org/gnome/shell/app-switcher/current-workspace-only" = true;
    };
  };
}




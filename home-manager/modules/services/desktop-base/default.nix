{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-desktop-base;
in
{
  options = {
    services.az-desktop-base.enable = lib.mkEnableOption "Base configuration for a destkop.";
  };

  config = lib.mkIf cfg.enable {
    programs = {
      az-alacritty.enable = true;
      az-czkawka.enable = true;
      az-evince.enable = true;
      az-keeweb.enable = true;
    };
    # raw config files
    home.file.".config/qtile/autostart.d/xdg-portal-add-path.sh".source = ./xdg-portal-add-path.sh;

    gtk = {
      cursorTheme = {
        package = pkgs.vanilla-dmz;
        name = "Vanilla-DMZ";
      };
      font = {
        name = "DejaVu Sans";
        package = pkgs.dejavu_fonts;
        size = 12;
      };
      theme = {
        name = "Adwaita";
        package = pkgs.gnome.gnome-themes-extra;
      };
    };

    xdg = {
      mimeApps = {
        enable = true;
        associations.added = {
          "x-scheme-handler/http" = "firefox.desktop";
          "x-scheme-handler/https" = "firefox.desktop";
          "x-scheme-handler/about" = "firefox.desktop";
          "x-scheme-handler/unknown" = "firefox.desktop";
          "x-scheme-handler/chrome" = "firefox.desktop";
          "text/html" = "firefox.desktop";
          "application/x-extension-htm" = "firefox.desktop";
          "application/x-extension-html" = "firefox.desktop";
          "application/x-extension-shtml" = "firefox.desktop";
          "application/xhtml+xml" = "firefox.desktop";
          "application/x-extension-xhtml" = "firefox.desktop";
          "application/x-extension-xht" = "firefox.desktop";
          "application/x-www-browser" = "firefox.desktop";
          "x-www-browser" = "firefox.desktop";
          "x-scheme-handler/webcal" = "firefox.desktop";
        };
        defaultApplications = {
          "x-scheme-handler/http" = "firefox.desktop";
          "x-scheme-handler/https" = "firefox.desktop";
          "x-scheme-handler/about" = "firefox.desktop";
          "x-scheme-handler/unknown" = "firefox.desktop";
          "x-scheme-handler/chrome" = "firefox.desktop";
          "text/html" = "firefox.desktop";
          "application/x-extension-htm" = "firefox.desktop";
          "application/x-extension-html" = "firefox.desktop";
          "application/x-extension-shtml" = "firefox.desktop";
          "application/xhtml+xml" = "firefox.desktop";
          "application/x-extension-xhtml" = "firefox.desktop";
          "application/x-extension-xht" = "firefox.desktop";
          "application/x-www-browser" = "firefox.desktop";
          "x-www-browser" = "firefox.desktop";
          "x-scheme-handler/webcal" = "firefox.desktop";
        };
      };
      userDirs = {
        enable = true;
        pictures = "${config.home.homeDirectory}/nextcloud/20_pictures";
        createDirectories = true;
      };
      # forcecully override the mimeapps.list
      # this is required because it isn't a file nix can easily lock
      # https://github.com/nix-community/home-manager/issues/1213
      configFile."mimeapps.list".force = true;
    };

    xsession = {
      numlock.enable = true;
    };

    services = {
      az-dunst.enable = true;
      az-espanso.enable = true;
      az-grobi.enable = true;
      network-manager-applet.enable = true;
      nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
    systemd.user.services.network-manager-applet = {
      Service = {
        Restart = "always";
      };
    };
    systemd.user.services.nextcloud-client = {
      Unit = {
        After = pkgs.lib.mkForce "graphical-session.target";
      };
    };
  };
}

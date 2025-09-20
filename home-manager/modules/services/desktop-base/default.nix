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

    services = {
      az-espanso.enable = true;
      nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
  };
}

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
          "x-scheme-handler/http" = "librewolf.desktop";
          "x-scheme-handler/https" = "librewolf.desktop";
          "x-scheme-handler/about" = "librewolf.desktop";
          "x-scheme-handler/unknown" = "librewolf.desktop";
          "x-scheme-handler/chrome" = "librewolf.desktop";
          "text/html" = "librewolf.desktop";
          "application/x-extension-htm" = "librewolf.desktop";
          "application/x-extension-html" = "librewolf.desktop";
          "application/x-extension-shtml" = "librewolf.desktop";
          "application/xhtml+xml" = "librewolf.desktop";
          "application/x-extension-xhtml" = "librewolf.desktop";
          "application/x-extension-xht" = "librewolf.desktop";
          "application/x-www-browser" = "librewolf.desktop";
          "x-www-browser" = "librewolf.desktop";
          "x-scheme-handler/webcal" = "librewolf.desktop";
        };
        defaultApplications = {
          "x-scheme-handler/http" = "librewolf.desktop";
          "x-scheme-handler/https" = "librewolf.desktop";
          "x-scheme-handler/about" = "librewolf.desktop";
          "x-scheme-handler/unknown" = "librewolf.desktop";
          "x-scheme-handler/chrome" = "librewolf.desktop";
          "text/html" = "librewolf.desktop";
          "application/x-extension-htm" = "librewolf.desktop";
          "application/x-extension-html" = "librewolf.desktop";
          "application/x-extension-shtml" = "librewolf.desktop";
          "application/xhtml+xml" = "librewolf.desktop";
          "application/x-extension-xhtml" = "librewolf.desktop";
          "application/x-extension-xht" = "librewolf.desktop";
          "application/x-www-browser" = "librewolf.desktop";
          "x-www-browser" = "librewolf.desktop";
          "x-scheme-handler/webcal" = "librewolf.desktop";
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

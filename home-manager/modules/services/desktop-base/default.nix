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
      az-mpv.enable = true;
      az-signal.enable = true;
      az-telegram.enable = true;
      bash = {
        shellAliases = {
          management-server = "mosh ${config.home.username}@10.7.89.150 -- tmux new -A -s 0";
          work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
        };
      };
    };
    home.packages = [
      pkgs.meld
      pkgs.nitrogen
      pkgs.libreoffice-fresh
      pkgs.remmina
    ];
    # raw config files
    home.file.".config/qtile/autostart.d/xdg-portal-add-path.sh".source = ./xdg-portal-add-path.sh;

    gtk.theme.name = "Arc-Darker";

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
    systemd.user.services.nextcloud-client = {
      Unit = {
        After = pkgs.lib.mkForce "graphical-session.target";
      };
    };
  };
}

{ config, lib, pkgs, ... }:
let
  cfg = config.profiles.az-desktop;
in
{
  options = {
    profiles.az-desktop.enable = lib.mkEnableOption "Enable desktop";
  };

  config = lib.mkIf cfg.enable {
    networking = {
      networkmanager.enable = true;
    };

    services = {
      az-docker.enable = true;
      az-pipewire.enable = true;
      az-qtile.enable = true;
      flatpak.enable = true;
      fwupd.enable = true;
      gnome = {
        gnome-keyring.enable = true;
        tracker.enable = true;
      };
      gvfs.enable = true;
      picom = {
        enable = true;
        vSync = true;
      };
      printing.enable = true;
      redshift.enable = true;
      udisks2.enable = true;
      # Enable the X11 windowing system.
      xserver = {
        enable = true;
        displayManager.lightdm.enable = true;
        layout = "us";
        xkbOptions = "compose:ralt";
        libinput = {
          enable = true;
          touchpad = {
            accelSpeed = "0.3";
            disableWhileTyping = true;
            scrollMethod = "twofinger";
          };
        };
      };
    };

    # taken from here: https://github.com/NixOS/nixpkgs/blob/nixos-22.11/nixos/modules/hardware/video/hidpi.nix
    # {
    # Needed when typing in passwords for full disk encryption
    console.earlySetup = true;
    boot.loader.systemd-boot.consoleMode = "1";
    # }

    fonts = {
      fontconfig.defaultFonts = {
        serif = [ "TeX Gyre Pagella" ];
        monospace = [ "Source Code Pro" ];
      };
      fonts = with pkgs; [
        gyre-fonts
        source-code-pro
      ];
    };

    # Enable keyring
    security.pam.services.lightdm.enableGnomeKeyring = true;

    # Enable sound.
    sound.enable = true;

    programs = {
      az-eog.enable = true; # Gnome Image Viewer
      az-hunspell.enable = true; # Enable dictionaries
      az-idevices.enable = true;
      az-nautilus.enable = true;
      az-nix-direnv.enable = true;
      az-scripts.enable = true;
    };

    xdg = {
      portal = {
        enable = true;
        xdgOpenUsePortal = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
      mime = {
        addedAssociations = {
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
    };
    environment = {
      systemPackages = with pkgs; [
        # what I consider to be system packages
        alacritty
        appimage-run
        brightnessctl
        firefox
        gnome.file-roller
        gnome.gnome-screenshot
        lm_sensors
        lxappearance
        networkmanager-openvpn
        p7zip
        pavucontrol
        quickemu
        rofi
        source-code-pro
        unrar
      ];
      variables = {
        WINIT_X11_SCALE_FACTOR = "1";
      };
      sessionVariables = {
        DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
        QT_SCALE_FACTOR = "1.25";
      };
    };
  };
}

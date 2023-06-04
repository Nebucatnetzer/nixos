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
      # Enable Flatpack
      flatpak.enable = true;
      fwupd.enable = true;
      gnome.gnome-keyring.enable = true;
      gvfs.enable = true;
      picom = {
        enable = true;
        vSync = true;
      };
      printing.enable = true;
      redshift = {
        enable = true;
      };
      # Enable the X11 windowing system.
      xserver = {
        enable = true;
        displayManager.lightdm.enable = true;
        displayManager.defaultSession = "none+qtile";
        windowManager.qtile.enable = true;
        layout = "us";
        xkbOptions = "compose:ralt";
        libinput.enable = true;
      };
    };

    # taken from here: https://github.com/NixOS/nixpkgs/blob/nixos-22.11/nixos/modules/hardware/video/hidpi.nix
    # {
    # Needed when typing in passwords for full disk encryption
    console.earlySetup = true;
    boot.loader.systemd-boot.consoleMode = "1";
    fonts.fontconfig = {
      antialias = true;
      subpixel = {
        rgba = "none";
        lcdfilter = "none";
      };
    };
    # }

    fonts.fonts = with pkgs; [
      source-code-pro
    ];

    # Enable keyring
    security.pam.services.lightdm.enableGnomeKeyring = true;

    # Enable sound.
    sound.enable = true;

    programs = {
      # Enable dconf to be able to save Nautilus settings
      dconf.enable = true;
      az-email.enable = true;
      # Gnome Image Viewer
      az-eog.enable = true;
      # Enable dictionaries
      az-hunspell.enable = true;
      az-idevices.enable = true;
      az-nix-direnv.enable = true;
      az-scripts.enable = true;
      az-tmux.enable = true;
    };

    xdg = {
      portal = {
        enable = true;
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
        lm_sensors
        lxappearance
        gnome.file-roller
        gnome.gnome-screenshot
        gnome.nautilus
        networkmanager-openvpn
        nitrogen
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
      };
    };
  };
}

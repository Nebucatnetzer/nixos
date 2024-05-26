{
  config,
  lib,
  pkgs,
  ...
}:
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

    documentation = {
      man.generateCaches = true;
      nixos.includeAllModules = true;
    };

    age.identityPaths = [ "/home/${config.az-username}/.ssh/id_rsa" ];
    services = {
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
      libinput = {
        enable = true;
        touchpad = {
          accelSpeed = "0.3";
          disableWhileTyping = true;
          scrollMethod = "twofinger";
        };
      };
      xserver = {
        enable = true;
        displayManager.lightdm.enable = true;
        xkb = {
          layout = "us";
          options = "compose:ralt";
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
      packages = [
        pkgs.gyre-fonts
        pkgs.source-code-pro
      ];
    };

    # Enable keyring
    security.pam.services.lightdm.enableGnomeKeyring = true;

    # Enable sound.
    sound.enable = true;

    programs = {
      az-eog.enable = true; # Gnome Image Viewer
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
        config = {
          common = {
            default = [ "gtk" ];
            "org.freedesktop.impl.portal.Secret" = [ "gnome-keyring" ];
          };
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

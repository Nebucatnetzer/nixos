{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.az-desktop;
  toggle-keyboard = pkgs.callPackage "${inputs.self}/pkgs/toggle-keyboard" { };
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
      az-clipcat.enable = true;
      az-pipewire.enable = true;
      flatpak.enable = true;
      fwupd.enable = true;
      gvfs.enable = true;
      picom = {
        enable = true;
        vSync = true;
      };
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
        serverFlagsSection = ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "0"
          Option "OffTime" "0"
        '';
        xkb = {
          layout = "us";
          options = "compose:caps";
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

    # Enable sound.
    sound.enable = true;

    programs = {
      az-eog.enable = true; # Gnome Image Viewer
      az-idevices.enable = true;
      az-nautilus.enable = true;
      az-nix-direnv.enable = true;
      az-scripts.enable = true;
    };
    hardware.keyboard.zsa.enable = true;

    environment = {
      systemPackages = [
        # what I consider to be system packages
        pkgs.alacritty
        pkgs.appimage-run
        pkgs.brightnessctl
        pkgs.firefox
        pkgs.lm_sensors
        pkgs.networkmanager-openvpn
        pkgs.p7zip
        pkgs.pavucontrol
        pkgs.podman-compose
        pkgs.quickemu
        pkgs.strawberry
        pkgs.unrar
        pkgs.wally-cli
        toggle-keyboard
      ];
      variables = {
        WINIT_X11_SCALE_FACTOR = "1";
      };
      sessionVariables = {
        DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
        QT_SCALE_FACTOR = "1.25";
      };
    };
    virtualisation.virtualbox.host.enable = true;
    virtualisation.podman.enable = true;
  };
}

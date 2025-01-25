{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-tiling-wm-base;
  az-lock-screen = pkgs.writeShellScriptBin "az-lock-screen" ''
    # Lock the screen
    ${pkgs.i3lock}/bin/i3lock -c 000000
    # Turn off the display
    ${pkgs.xorg.xset}/bin/xset dpms force off

    # If we are battery kill the radios
    ac_powerstatus_file=/sys/class/power_supply/AC/online
    if [ -e "$ac_powerstatus_file" ]; then
      ac_powerstatus=$(<$ac_powerstatus_file)
      if [ $ac_powerstatus != 1 ]; then
        ${pkgs.util-linux}/bin/rfkill block all
      fi
    fi
  '';
in
{
  options = {
    services.az-tiling-wm-base.enable = lib.mkEnableOption "Enable basic utilities for Window Managers";
  };
  config = lib.mkIf cfg.enable {
    home-manager.users.${config.az-username} = {
      services.az-xidlehook.enable = true;
    };
    environment = {
      systemPackages = [
        pkgs.file-roller
        pkgs.gnome-screenshot
        pkgs.lxappearance
        pkgs.rofi
      ];
    };
    programs = {
      xss-lock = {
        enable = true;
        lockerCommand = "${az-lock-screen}/bin/az-lock-screen";
      };
    };
    services = {
      displayManager.ly = {
        enable = true;
        settings = {
          save = true; # Save current desktop & user
          load = true; # Load saved desktop & user
        };
      };
      gnome = {
        gnome-keyring.enable = true;
        tinysparql.enable = true;
      };
      redshift.enable = true;
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
  };
}

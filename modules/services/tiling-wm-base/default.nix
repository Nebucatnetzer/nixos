{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-tiling-wm-base;
  az-lock-screen = pkgs.writeShellScriptBin "az-lock-screen" ''
    ${pkgs.i3lock}/bin/i3lock -c 000000
    ${pkgs.xorg.xset}/bin/xset dpms force off
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

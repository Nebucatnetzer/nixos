{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-tiling-wm-base;
in
{
  options = {
    services.az-tiling-wm-base.enable = lib.mkEnableOption "Enable basic utilities for Window Managers";
  };
  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [
        pkgs.file-roller
        pkgs.gnome-screenshot
        pkgs.lxappearance
        pkgs.rofi
      ];
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

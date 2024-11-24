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
    # Enable keyring
    security.pam.services.lightdm.enableGnomeKeyring = true;
    services = {
      gnome = {
        gnome-keyring.enable = true;
        tinysparql.enable = true;
      };
      redshift.enable = true;
      xserver = {
        displayManager.lightdm.enable = true;
      };
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

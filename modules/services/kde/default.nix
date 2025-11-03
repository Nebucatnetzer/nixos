{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-kde;
in
{
  options = {
    services.az-kde.enable = lib.mkEnableOption "Enable KDE";
  };

  config = lib.mkIf cfg.enable {
    environment = {
      plasma6.excludePackages = with pkgs.kdePackages; [
        elisa
        kate
      ];
      systemPackages = [
        pkgs.kdePackages.audiocd-kio

        # caldav/cardav
        pkgs.kdePackages.akonadi # backend for PIM
        pkgs.kdePackages.akonadi-calendar
        pkgs.kdePackages.akonadi-calendar-tools
        pkgs.kdePackages.kaddressbook
        pkgs.kdePackages.kdepim-addons # display calendar events in the taskbar calendar
        pkgs.kdePackages.kdepim-runtime # backend for PIM
        pkgs.kdePackages.korganizer # required to connect to caldav
        pkgs.kdePackages.merkuro # calendar

        pkgs.kdePackages.kauth
        pkgs.kdePackages.kwallet-pam # for kwallet automatic login
        pkgs.kdePackages.kde-gtk-config
        pkgs.krename
      ];
    };
    services = {
      desktopManager.plasma6.enable = true;
      displayManager.sddm.wayland.enable = true;
      displayManager.sddm.enable = true;
    };
    programs.xwayland.enable = true;
    programs.partition-manager.enable = true;
  };
}

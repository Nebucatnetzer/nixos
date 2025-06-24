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
      ];
    };
    services = {
      desktopManager.plasma6.enable = true;
      displayManager.sddm.wayland.enable = true;
      displayManager.sddm.enable = true;
    };
    programs.xwayland.enable = true;
  };
}

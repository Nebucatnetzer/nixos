{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-droidcam;
in
{
  options = {
    programs.az-droidcam.enable = lib.mkEnableOption "Enable Droidcam.";
  };

  config = lib.mkIf cfg.enable {
    programs.droidcam.enable = true;
    # required for USB connection
    services.usbmuxd.enable = true;
    environment.shellAliases = {
      webcam = "droidcam-cli -size=1920x1080 ios 4747";
    };
    home-manager.users.${config.az-username} = {
      xdg.desktopEntries = {
        droidcam = {
          name = "Droidcam";
          exec = "${pkgs.droidcam}/bin/droidcam";
          terminal = false;
          categories = [
            "Video"
            "AudioVideo"
          ];
        };
      };
    };
  };
}

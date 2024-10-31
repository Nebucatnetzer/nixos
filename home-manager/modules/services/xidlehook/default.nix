{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-xidlehook;
in
{
  options = {
    services.az-xidlehook.enable = lib.mkEnableOption "Enable xidlehook.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.i3lock ];
    services.xidlehook = {
      enable = true;
      not-when-audio = true;
      not-when-fullscreen = true;
      timers = [
        {
          delay = 300;
          command = "${pkgs.brightnessctl}/bin/brightnessctl --quiet set 20%";
          canceller = "${pkgs.brightnessctl}/bin/brightnessctl --quiet set 50%";
        }
        {
          delay = 600;
          command = "${pkgs.brightnessctl}/bin/brightnessctl --quiet set 50%; ${pkgs.i3lock}/bin/i3lock -c 000000; ${pkgs.xorg.xset}/bin/xset dpms force off";
          canceller = "";
        }
      ];
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-telegram;
in
{
  options = {
    programs.az-telegram.enable = lib.mkEnableOption "Enable Telegram.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ unstable.tdesktop ];

    home.file.".config/qtile/autostart.d/telegram.sh".source = ./telegram.sh;
  };
}

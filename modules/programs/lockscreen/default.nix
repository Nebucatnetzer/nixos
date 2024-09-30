{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-lockscreen;
in
{
  options = {
    programs.az-lockscreen.enable = lib.mkEnableOption "Lockscreen";
  };

  config = lib.mkIf cfg.enable {
    programs.xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.i3lock}/bin/i3lock -c 000000";
    };

    environment.systemPackages = [ pkgs.i3lock ];
  };
}

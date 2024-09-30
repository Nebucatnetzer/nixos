{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hardware.az-dvd;
in
{
  options = {
    hardware.az-dvd.enable = lib.mkEnableOption "DVD";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.libaacs
      pkgs.libbluray
      pkgs.libdvdcss
      pkgs.libdvdnav
      pkgs.libdvdread
    ];
  };
}

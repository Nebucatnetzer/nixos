{ config, lib, pkgs, ... }:
let
  cfg = config.hardware.dvd;
in
{
  options = {
    hardware.dvd.enable = lib.mkEnableOption "DVD";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libaacs
      libbluray
      libdvdcss
      libdvdnav
      libdvdread
    ];
  };
}

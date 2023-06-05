{ config, lib, pkgs, ... }:
let
  cfg = config.hardware.az-dvd;
in
{
  options = {
    hardware.az-dvd.enable = lib.mkEnableOption "DVD";
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

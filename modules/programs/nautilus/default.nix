{ config, lib, pkgs, ... }:
let cfg = config.programs.az-nautilus;
in {
  options = {
    programs.az-nautilus.enable = lib.mkEnableOption "Enable Nautilus";
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        ffmpegthumbnailer
        gnome.nautilus
        libheif.bin
        libheif.out
        nufraw
      ];
    };
    programs = {
      dconf.enable = true; # Enable dconf to be able to save Nautilus settings
    };
  };
}

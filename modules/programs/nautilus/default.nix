{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-nautilus;
in
{
  options = {
    programs.az-nautilus.enable = lib.mkEnableOption "Enable Nautilus";
  };

  config = lib.mkIf cfg.enable {
    environment.pathsToLink = [ "share/thumbnailers" ];
    environment = {
      systemPackages = [
        pkgs.ffmpegthumbnailer
        pkgs.gnome.nautilus
        pkgs.libheif.bin
        pkgs.libheif.out
        pkgs.nufraw
      ];
    };
    programs = {
      dconf.enable = true; # Enable dconf to be able to save Nautilus settings
    };
  };
}

{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-rapid-photo-downloader;
in
{
  options = {
    programs.az-rapid-photo-downloader.enable = lib.mkEnableOption "Enable Rapid Photo Downloader.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.rapid-photo-downloader
    ];
    home.file.".config/Rapid Photo Downloader".source = ./config;
  };
}

{ pkgs, ... }:
{
  home.packages = with pkgs; [
    rapid-photo-downloader
  ];
  home.file.".config/Rapid Photo Downloader".source = ./config;
}

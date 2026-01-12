{ config, pkgs, ... }:
{
  home.packages = [ pkgs.rapid-photo-downloader ];
  home.file.".config/Rapid Photo Downloader/Rapid Photo Downloader.conf".source =
    config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/rapid-photo-downloader/config/Rapid Photo Downloader.conf";
}

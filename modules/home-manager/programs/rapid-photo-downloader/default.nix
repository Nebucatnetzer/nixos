{ config, pkgs, ... }:
{
  home.packages = [ pkgs.rapid-photo-downloader ];

  xdg.desktopEntries.rapid-photo-downloader = {
    name = "Rapid Photo Downloader";
    genericName = "Photo Downloader";
    comment = "Download, rename and back up photos and videos from cameras and other devices";
    exec = "${pkgs.rapid-photo-downloader}/bin/rapid-photo-downloader %f";
    # Absolute path to the bundled logo; templated to the store path. No
    # theme-icon named "rapid-photo-downloader" ships, so a bare name would not
    # resolve. The python3.13 segment must track the package's python version.
    icon = "${pkgs.rapid-photo-downloader}/lib/python3.13/site-packages/raphodo/data/rapid-photo-downloader.svg";
    terminal = false;
    categories = [
      "Graphics"
      "Photography"
    ];
    mimeType = [ "x-content/image-dcf" ];
    startupNotify = false;
    settings.StartupWMClass = "rapid-photo-downloader";
  };

  home.file.".config/Rapid Photo Downloader/Rapid Photo Downloader.conf".source =
    config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/rapid-photo-downloader/config/Rapid Photo Downloader.conf";
}

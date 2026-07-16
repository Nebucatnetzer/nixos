{ pkgs, ... }:
let
  rapid = pkgs.rapid-photo-downloader;
  rapidDesktop = pkgs.makeDesktopItem {
    name = "rapid-photo-downloader";
    desktopName = "Rapid Photo Downloader";
    genericName = "Photo Downloader";
    comment = "Download, rename and back up photos and videos from cameras and other devices";
    exec = "${rapid}/bin/rapid-photo-downloader %f";
    # Absolute path to the bundled logo; templated to the store path. No
    # theme-icon named "rapid-photo-downloader" ships, so a bare name would not
    # resolve. The python3.13 segment must track the package's python version.
    icon = "${rapid}/lib/python3.13/site-packages/raphodo/data/rapid-photo-downloader.svg";
    terminal = false;
    categories = [
      "Graphics"
      "Photography"
    ];
    mimeTypes = [ "x-content/image-dcf" ];
    startupNotify = false;
    startupWMClass = "rapid-photo-downloader";
  };
in
{
  environment.systemPackages = [
    rapid
    rapidDesktop
  ];
}

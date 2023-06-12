{ inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];
  home = {
    packages = with pkgs; [
      digikam
      exercism
      freetube
      chromium
      plexamp
      shotwell
      sound-juicer
      unstable.tagger
    ];
  };

  programs = {
    az-calibre.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-work-desktop.enable = true;
    az-yt-dlp.enable = true;
  };

  services = {
    az-desktop-base.enable = true;
  };
}


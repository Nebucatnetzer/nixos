{ inputs, pkgs, ... }:
let
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        nvidia-offload mpv --shuffle "/run/media/andreas/e7c05e7e-79ec-45af-b1c8-840020fcf708/$directory/" &
    done
  '';
in
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
      az-media
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


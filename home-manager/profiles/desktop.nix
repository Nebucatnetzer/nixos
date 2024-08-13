{ inputs, pkgs, ... }:
let
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        nvidia-offload mpv --shuffle --mute=yes "/run/media/andreas/various/$directory/" &
    done
  '';
  send-to-kindle = pkgs.callPackage "${inputs.self}/pkgs/send-to-kindle" { };
in
{
  imports = [ ./management.nix ];
  home = {
    packages = [
      pkgs.chromium # needed for cloud gaming
      pkgs.digikam
      pkgs.exercism
      pkgs.freetube
      pkgs.plexamp
      pkgs.sound-juicer
      pkgs.unstable.tagger
      az-media
      send-to-kindle
    ];
  };

  programs = {
    az-beets.enable = true;
    az-calibre.enable = true;
    az-copilot-cli.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-work-desktop.enable = true;
    az-yt-dlp.enable = true;
    az-xcompose.enable = true;
  };

  services = {
    az-attic-client.enable = true;
    az-desktop-base.enable = true;
  };
}

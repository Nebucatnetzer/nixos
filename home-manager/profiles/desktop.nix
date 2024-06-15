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
    packages = with pkgs; [
      chromium # needed for cloud gaming
      digikam
      exercism
      freetube
      nodejs # needed for ansible-language-server
      plexamp
      send-to-kindle
      sound-juicer
      unstable.tagger
      az-media
    ];
  };

  programs = {
    az-calibre.enable = true;
    az-copilot-cli.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-work-desktop.enable = true;
    az-yt-dlp.enable = true;
  };

  services = {
    az-attic-client.enable = true;
    az-desktop-base.enable = true;
  };
}

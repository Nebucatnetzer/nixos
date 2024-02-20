{ inputs, pkgs, ... }:
let
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        nvidia-offload mpv --shuffle --mute=yes "/run/user/1000/gvfs/smb-share:server=10.7.89.108,share=various/$directory/" &
    done
  '';
in
{
  imports = [ "${inputs.self}/home-manager/profiles/management.nix" ];
  home = {
    packages = with pkgs; [
      chromium # needed for cloud gaming
      digikam
      exercism
      freetube
      nodejs # needed for ansible-language-server
      plexamp
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

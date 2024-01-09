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
in {
  imports = [ "${inputs.self}/home-manager/modules" ];
  home = {
    packages = with pkgs; [
      digikam
      exercism
      freetube
      chromium
      nodejs # needed for ansible-language-server
      plexamp
      sound-juicer
      unstable.tagger
      az-media
    ];
    shellAliases = {
      unlock-luks =
        "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o User=root";
    };
  };

  programs = {
    az-calibre.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-work-desktop.enable = true;
    az-yt-dlp.enable = true;
  };

  services = { az-desktop-base.enable = true; };
}


{ inputs, pkgs, ... }:
let
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        nvidia-offload mpv --shuffle --mute=yes "/run/user/1000/gvfs/smb-share:server=10.7.89.108,share=various2/$directory/" &
    done
  '';
  unlock-luks = pkgs.writeShellScriptBin "unlock-luks" ''
    until ${pkgs.netcat}/bin/nc -vzw 2 $1 22; do
        sleep 1
    done &&
        ${pkgs.openssh}/bin/ssh \
          -o UserKnownHostsFile=/dev/null \
          -o StrictHostKeyChecking=no \
          -o User=root \
          $1
  '';
in
{
  imports = [ "${inputs.self}/home-manager/profiles/management.nix" ];
  home = {
    packages = with pkgs; [
      digikam
      exercism
      freetube
      nodejs # needed for ansible-language-server
      plexamp
      sound-juicer
      unstable.tagger
      az-media
      unlock-luks
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

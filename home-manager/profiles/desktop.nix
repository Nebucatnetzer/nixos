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
  dap-sync = pkgs.callPackage "${inputs.self}/pkgs/dap-sync" { };
  freetube = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.freetube;
  send-to-kindle = pkgs.callPackage "${inputs.self}/pkgs/send-to-kindle" { };
  unstable-pkgs = import inputs.nixpkgs-unstable {
    system = pkgs.system;
    config = {
      allowUnfree = true;
    };
  };
in
{
  imports = [ ./management.nix ];
  home = {
    packages = [
      pkgs.chromium # needed for cloud gaming
      pkgs.digikam
      pkgs.exercism
      pkgs.plexamp
      pkgs.sound-juicer
      pkgs.tagger
      az-media
      dap-sync
      freetube
      send-to-kindle
      unstable-pkgs.vscode
    ];
  };

  programs = {
    az-beets.enable = true;
    az-calibre.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-work-desktop.enable = true;
    az-yt-dlp.enable = true;
  };

  services = {
    az-desktop-base.enable = true;
    az-xidlehook.enable = true;
  };
}

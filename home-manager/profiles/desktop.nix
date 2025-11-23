{
  config,
  inputs,
  pkgs,
  ...
}:
let
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        mpv --shuffle --mute=yes "/run/media/andreas/various/$directory/" &
    done
  '';
  send-to-kindle = pkgs.callPackage "${inputs.self}/pkgs/send-to-kindle" { };
in
{
  imports = [ ./management.nix ];
  home = {
    file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

    packages = [
      # photographie packages
      pkgs.art
      pkgs.digikam

      # wine
      pkgs.bottles
      pkgs.wine-wayland
      pkgs.wineWowPackages.waylandFull

      pkgs.chromium # needed for cloud gaming and flashing zsa
      pkgs.czkawka
      pkgs.exercism
      pkgs.keepassxc
      pkgs.libreoffice-qt-fresh
      pkgs.meld
      pkgs.plexamp
      pkgs.plex-desktop
      pkgs.remmina
      pkgs.tagger
      az-media
      send-to-kindle
    ];
  };

  programs = {
    az-beets.enable = true;
    az-calibre.enable = true;
    az-mpv.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-signal.enable = true;
    az-telegram.enable = true;
    az-work-desktop.enable = true;
    bash = {
      shellAliases = {
        management-server = "mosh ${config.home.username}@10.7.89.153 -- tmux new -A -s 0";
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
      };
    };
  };

}

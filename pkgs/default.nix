{ pkgs }:
{
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        mpv --shuffle --mute=yes "/run/media/andreas/various/$directory/" &
    done
  '';
  date-to-filename = pkgs.callPackage ./date-to-filename { };
  denote-rename = pkgs.callPackage ./denote-rename { };
  send-to-kindle = pkgs.callPackage ./send-to-kindle { };
  toggle-keyboard = pkgs.callPackage ./toggle-keyboard { };
  update-file-dates = pkgs.callPackage ./update-file-dates { };
  rebuild = pkgs.callPackage ./rebuild { };
  unlock-luks = pkgs.callPackage ./unlock-luks { };
}

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
  buildRaspiImage = pkgs.callPackage ./build-raspi-image { };
  date-to-filename = pkgs.callPackage ./date-to-filename { };
  denote-rename = pkgs.callPackage ./denote-rename { };
  send-to-kindle = pkgs.callPackage ./send-to-kindle { };
  toggle-keyboard = pkgs.callPackage ./toggle-keyboard { };
  update-file-dates = pkgs.callPackage ./update-file-dates { };
  raspi4Uefi = pkgs.callPackage ./raspi4-uefi { };
  raspiVideoOutput = pkgs.callPackage ./raspi-video-output { };
  rebuild = pkgs.callPackage ./rebuild { };
  testDiskoImage = pkgs.callPackage ./test-disko-image { };
  unlock-luks = pkgs.callPackage ./unlock-luks { };
}

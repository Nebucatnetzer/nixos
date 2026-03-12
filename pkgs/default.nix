{ inputs, pkgs }:
let
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system};
  yt-dlp = unstable.yt-dlp;
in
rec {
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        mpv --shuffle --mute=yes "/run/media/andreas/various/$directory/" &
    done
  '';
  build-raspi-image = pkgs.callPackage ./build-raspi-image { };
  custom-mpv = pkgs.callPackage ./mpv { };
  dap-sync = pkgs.callPackage ./dap-sync { };
  date-to-filename = pkgs.callPackage ./date-to-filename { };
  denote-rename = pkgs.callPackage ./denote-rename { };
  download-articles = pkgs.callPackage ./download-articles { };
  download-video = pkgs.callPackage ./download-video { inherit yt-dlp; };
  download-playlist = pkgs.callPackage ./download-playlist { inherit yt-dlp; };
  jdownloader = pkgs.callPackage ./jdownloader { };
  raspi-video-output = pkgs.callPackage ./raspi-video-output { };
  raspi4-uefi = pkgs.callPackage ./raspi4-uefi { };
  rebuild = pkgs.callPackage ./rebuild { };
  test-disko-image = pkgs.callPackage ./test-disko-image { };
  toggle-keyboard = pkgs.callPackage ./toggle-keyboard { };
  unlock-luks = pkgs.callPackage ./unlock-luks { };
  update-file-dates = pkgs.callPackage ./update-file-dates { };
  video-to-mpv = pkgs.callPackage ./video-to-mpv {
    inherit yt-dlp;
    mpv = custom-mpv;
  };
  watch-playlist = pkgs.callPackage ./watch-playlist { mpv = custom-mpv; };
  watch-video = pkgs.callPackage ./watch-video { mpv = custom-mpv; };
}

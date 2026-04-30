{
  pkgs,
  unstable-pkgs,
}:
let
  yt-dlp = unstable-pkgs.yt-dlp;
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
  custom-mpv = pkgs.callPackage ./mpv { };
  dap-sync = pkgs.callPackage ./dap-sync { };
  date-to-filename = pkgs.callPackage ./date-to-filename { };
  denote-rename = pkgs.callPackage ./denote-rename { };
  download-articles = pkgs.callPackage ./download-articles { };
  download-video = pkgs.callPackage ./download-video { inherit yt-dlp; };
  download-playlist = pkgs.callPackage ./download-playlist { inherit yt-dlp; };
  jdownloader = pkgs.callPackage ./jdownloader { };
  rebuild = pkgs.callPackage ./rebuild { };
  toggle-keyboard = pkgs.callPackage ./toggle-keyboard { };
  unlock-luks = pkgs.callPackage ./unlock-luks { };
  update-file-dates = pkgs.callPackage ./update-file-dates { };
  video-to-mpv = pkgs.callPackage ./video-to-mpv {
    inherit yt-dlp;
    mpv = custom-mpv;
  };
  watch-playlist = pkgs.callPackage ./watch-playlist { mpv = custom-mpv; };
  watch-random-video = pkgs.callPackage ./watch-random-video { mpv = custom-mpv; };
  watch-video = pkgs.callPackage ./watch-video { mpv = custom-mpv; };
  win32yank = pkgs.callPackage ./win32yank { inherit (pkgs) pkgsCross; };
}

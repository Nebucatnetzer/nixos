{
  pkgs,
  unstable-pkgs,
}:
let
  inherit (unstable-pkgs) yt-dlp;
  mediaPaths = import ./mediaPaths.nix;
in
rec {
  az-media =
    pkgs.lib.meta.addMetaAttrs
      {
        description = "Open four shuffled mpv instances for ambient video playback";
        license = pkgs.lib.licenses.gpl3Plus;
        mainProgram = "az-media";
        platforms = pkgs.lib.platforms.linux;
      }
      (
        pkgs.writeShellScriptBin "az-media" ''
          videos="videos"
          directory="''${1:-videos}"
          for i in $(seq 1 4);
          do
              mpv --shuffle --mute=yes "${mediaPaths.variousVideos}/$directory/" &
          done
        ''
      );
  custom-mpv = pkgs.callPackage ./mpv { };
  dap-sync = pkgs.callPackage ./dap-sync { };
  date-to-filename = pkgs.callPackage ./date-to-filename { };
  denote-rename = pkgs.callPackage ./denote-rename { };
  download-articles = pkgs.callPackage ./download-articles { };
  download-video = pkgs.callPackage ./download-video { inherit yt-dlp mediaPaths; };
  download-playlist = pkgs.callPackage ./download-playlist { inherit yt-dlp mediaPaths; };
  dptfxtract = pkgs.callPackage ./dptfxtract { };
  jdownloader = pkgs.callPackage ./jdownloader { inherit mediaPaths; };
  rebuild = pkgs.callPackage ./rebuild { };
  sidecar-cleanup = pkgs.callPackage ./sidecar-cleanup { };
  toggle-keyboard = pkgs.callPackage ./toggle-keyboard { };
  unlock-luks = pkgs.callPackage ./unlock-luks { };
  update-file-dates = pkgs.callPackage ./update-file-dates { };
  video-to-mpv = pkgs.callPackage ./video-to-mpv {
    inherit yt-dlp;
    mpv = custom-mpv;
  };
  watch-playlist = pkgs.callPackage ./watch-playlist {
    mpv = custom-mpv;
    inherit mediaPaths;
  };
  watch-random-video = pkgs.callPackage ./watch-random-video {
    mpv = custom-mpv;
    inherit mediaPaths;
  };
  watch-video = pkgs.callPackage ./watch-video {
    mpv = custom-mpv;
    inherit mediaPaths;
  };
  win32yank = pkgs.callPackage ./win32yank { inherit (pkgs) pkgsCross; };
  zapp = pkgs.callPackage ./zapp { };
}

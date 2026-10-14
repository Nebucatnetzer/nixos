{
  ffmpeg-headless,
  lib,
  mediaPaths,
  python3Packages,
  writers,
}:
lib.meta.addMetaAttrs
  {
    description = "Download a watch-later playlist into the main video folder, skipping videos that do not fit";
    license = lib.licenses.gpl3Plus;
    mainProgram = "download-watch-later";
    platforms = lib.platforms.linux;
  }
  (
    writers.writePython3Bin "download-watch-later" {
      flakeIgnore = [ "E501" ];
      libraries = [ python3Packages.yt-dlp ];
      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath [ ffmpeg-headless ])
        "--add-flag"
        "--destination=${mediaPaths.youtubeVideos}"
        "--add-flag"
        "--video-root=${mediaPaths.youtubeVideos}"
        "--add-flag"
        "--mode=watch-later"
      ];
    } (builtins.readFile ./download_video.py)
  )

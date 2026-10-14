{
  ffmpeg-headless,
  lib,
  mediaPaths,
  python3Packages,
  writers,
}:
lib.meta.addMetaAttrs
  {
    description = "Download a single video via yt-dlp using Firefox cookies";
    license = lib.licenses.gpl3Plus;
    mainProgram = "download-video";
    platforms = lib.platforms.linux;
  }
  (
    writers.writePython3Bin "download-video" {
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
      ];
    } (builtins.readFile ./download_video.py)
  )

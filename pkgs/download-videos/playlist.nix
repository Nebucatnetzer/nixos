{
  ffmpeg-headless,
  lib,
  mediaPaths,
  python3Packages,
  writers,
}:
lib.meta.addMetaAttrs
  {
    description = "Download a YouTube playlist via yt-dlp using Firefox cookies";
    license = lib.licenses.gpl3Plus;
    mainProgram = "download-playlist";
    platforms = lib.platforms.linux;
  }
  (
    writers.writePython3Bin "download-playlist" {
      flakeIgnore = [ "E501" ];
      libraries = [ python3Packages.yt-dlp ];
      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath [ ffmpeg-headless ])
        "--add-flag"
        "--destination=${mediaPaths.youtubePlaylists}"
        "--add-flag"
        "--video-root=${mediaPaths.youtubeVideos}"
        "--add-flag"
        "--mode=playlist"
      ];
    } (builtins.readFile ./download_video.py)
  )

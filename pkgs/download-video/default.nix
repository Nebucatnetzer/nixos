{
  lib,
  mediaPaths,
  yt-dlp,
  writeShellApplication,
}:
writeShellApplication {
  name = "download-video";
  runtimeInputs = [
    yt-dlp
  ];
  meta = {
    description = "Download a single video via yt-dlp using Firefox cookies";
    license = lib.licenses.gpl3Plus;
    mainProgram = "download-video";
    platforms = lib.platforms.linux;
  };
  text = ''
    yt-dlp \
      --cookies-from-browser firefox \
      --no-playlist \
      --remux-video=mkv \
      --paths=${mediaPaths.youtubeIndependentVideos}/ \
      "$@"
  '';
}

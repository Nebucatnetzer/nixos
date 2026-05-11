{
  lib,
  mediaPaths,
  yt-dlp,
  writeShellApplication,
}:
writeShellApplication {
  name = "download-playlist";
  runtimeInputs = [
    yt-dlp
  ];
  meta = {
    description = "Download a YouTube playlist via yt-dlp using Firefox cookies";
    license = lib.licenses.gpl3Plus;
    mainProgram = "download-playlist";
    platforms = lib.platforms.linux;
  };
  text = ''
    yt-dlp \
      --cookies-from-browser firefox \
      --yes-playlist \
      --remux-video=mkv \
      --paths=${mediaPaths.youtubePlaylists}/ \
      --output="%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" \
      "$@"
  '';
}

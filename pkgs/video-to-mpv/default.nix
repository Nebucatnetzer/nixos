{
  lib,
  mpv,
  yt-dlp,
  writeShellApplication,
}:
writeShellApplication {
  name = "video-to-mpv";
  runtimeInputs = [
    mpv
    yt-dlp
  ];
  meta = {
    description = "Download a YouTube video to a temp dir and immediately open it in mpv";
    license = lib.licenses.gpl3Plus;
    mainProgram = "video-to-mpv";
    platforms = lib.platforms.linux;
  };
  text = ''
    tmpDir=$(mktemp -d);
    yt-dlp \
      --cookies-from-browser firefox \
      --no-playlist \
      --remux-video=mkv \
      --paths="$tmpDir" \
      "$@";
    mpv "$tmpDir"/*.mkv;
  '';
}

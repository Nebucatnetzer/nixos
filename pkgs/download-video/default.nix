{
  yt-dlp,
  writeShellApplication,
}:
writeShellApplication {
  name = "download-video";
  runtimeInputs = [
    yt-dlp
  ];
  text = ''
    yt-dlp \
      --cookies-from-browser firefox \
      --no-playlist \
      --remux-video=mkv \
      --paths=~/Videos/youtube/independent_videos/ \
      "$@"
  '';
}

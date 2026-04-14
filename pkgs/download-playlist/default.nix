{
  yt-dlp,
  writeShellApplication,
}:
writeShellApplication {
  name = "download-playlist";
  runtimeInputs = [
    yt-dlp
  ];
  text = ''
    yt-dlp \
      --cookies-from-browser firefox \
      --yes-playlist \
      --remux-video=mkv \
      --paths=/run/media/andreas/20260414--ext-ssd/videos/youtube/playlists/ \
      --output="%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" \
      "$@"
  '';
}

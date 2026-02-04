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
      --paths=~/Downloads/youtube/playlists/ \
      --output="%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s" \
      "$@"
  '';
}

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
      --preset-alias=mkv \
      --paths=~/Downloads/youtube/ \
      "$@"
  '';
}

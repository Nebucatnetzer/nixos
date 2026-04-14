{
  mpv,
  writeShellApplication,
}:
writeShellApplication {
  name = "watch-random-video";
  runtimeInputs = [
    mpv
  ];
  text = ''
    mpv --save-position-on-quit \
        --shuffle /run/media/andreas/20260414--ext-ssd/videos/youtube/independent_videos/
  '';
}

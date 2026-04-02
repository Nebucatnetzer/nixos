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
        --shuffle ~/Videos/youtube/independent_videos/
  '';
}

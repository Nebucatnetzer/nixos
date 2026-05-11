{
  lib,
  mediaPaths,
  mpv,
  writeShellApplication,
}:
writeShellApplication {
  name = "watch-random-video";
  runtimeInputs = [
    mpv
  ];
  meta = {
    description = "Play all independent videos from the external SSD in random order";
    license = lib.licenses.gpl3Plus;
    mainProgram = "watch-random-video";
    platforms = lib.platforms.linux;
  };
  text = ''
    mpv --save-position-on-quit \
        --shuffle ${mediaPaths.youtubeIndependentVideos}/
  '';
}

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
    # --shuffle makes the default --directory-mode recursive, which would drag in
    # every playlist video, so the immediate children are asked for explicitly.
    mpv --save-position-on-quit \
        --directory-mode=lazy \
        --shuffle ${mediaPaths.youtubeVideos}/
  '';
}

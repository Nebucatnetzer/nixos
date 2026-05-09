{
  lib,
  fd,
  fzf,
  mpv,
  writeShellApplication,
}:
writeShellApplication {
  name = "watch-video";
  runtimeInputs = [
    fd
    fzf
    mpv
  ];
  meta = {
    description = "Fuzzy-pick an independent video from the external SSD and play it in mpv";
    license = lib.licenses.gpl3Plus;
    mainProgram = "watch-video";
    platforms = lib.platforms.linux;
  };
  text = ''
    fd \
      --search-path /run/media/andreas/20260414--ext-ssd/videos/youtube/independent_videos/ \
      --type file \
      --no-hidden \
      --max-depth 1 |
        fzf \
          --bind 'enter:become(mpv --save-position-on-quit {})'
  '';
}

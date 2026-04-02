{
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
  text = ''
    fd \
      --search-path ~/Videos/youtube/independent_videos/ \
      --type file \
      --no-hidden \
      --max-depth 1 |
        fzf \
          --bind 'enter:become(mpv --save-position-on-quit {})'
  '';
}

{
  fd,
  fzf,
  mpv,
  writeShellApplication,
}:
writeShellApplication {
  name = "watch-playlist";
  runtimeInputs = [
    fd
    fzf
    mpv
  ];
  text = ''
    fd \
      --search-path ~/Videos/youtube/playlists/ \
      --type directory \
      --no-hidden \
      --max-depth 1 |
        fzf \
          --bind 'enter:become(mpv --save-position-on-quit {})'
  '';
}

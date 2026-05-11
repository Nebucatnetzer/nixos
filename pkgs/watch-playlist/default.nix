{
  lib,
  fd,
  fzf,
  mediaPaths,
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
  meta = {
    description = "Fuzzy-pick a downloaded playlist from the external SSD and play it in mpv";
    license = lib.licenses.gpl3Plus;
    mainProgram = "watch-playlist";
    platforms = lib.platforms.linux;
  };
  text = ''
    fd \
      --search-path ${mediaPaths.youtubePlaylists}/ \
      --type directory \
      --no-hidden \
      --max-depth 1 |
        fzf \
          --bind 'enter:become(mpv --save-position-on-quit {})'
  '';
}

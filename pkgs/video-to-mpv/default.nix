{
  mpv,
  yt-dlp,
  writeShellApplication,
}:
writeShellApplication {
  name = "video-to-mpv";
  runtimeInputs = [
    mpv
    yt-dlp
  ];
  text = ''
    # download a single video from youtube, using cookies from firefox to bypass paywalls and age restrictions
    # store it in a tmp directory and then open it with mpv
    tmpDir=$(mktemp -d);
    yt-dlp \
      --cookies-from-browser firefox \
      --no-playlist \
      --remux-video=mkv \
      --paths="$tmpDir" \
      "$@";
    mpv "$tmpDir"/*.mkv;
  '';
}

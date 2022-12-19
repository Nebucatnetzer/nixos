{ ... }: {
  programs.yt-dlp = {
    enable = true;
    extraConfig = ''
      -S "+codec:h264"
    '';
    settings = {
      remux-video = "mkv";
    };
  };
}

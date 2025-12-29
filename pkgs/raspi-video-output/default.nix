{ mpv, writeShellApplication }:
writeShellApplication {
  name = "raspi-video-output";
  runtimeInputs = [
    mpv
  ];
  # https://github.com/mpv-player/mpv/wiki/Video4Linux2-Input
  text = ''
    mpv \
      --demuxer-lavf-o=video_size=1920x1080,input_format=mjpeg,framerate=60 \
      --profile=low-latency \
      av://v4l2:"$1"
  '';
}

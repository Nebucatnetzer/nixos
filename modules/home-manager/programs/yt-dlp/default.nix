{
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
{
  programs.yt-dlp = {
    enable = true;
    package = unstable-pkgs.yt-dlp;
    settings = {
      remux-video = "mkv";
    };
  };
}

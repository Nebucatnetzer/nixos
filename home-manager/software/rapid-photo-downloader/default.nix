{ unstable }: { ... }:
{
  home.packages = [
    unstable.rapid-photo-downloader
  ];
  home.file.".config/Rapid Photo Downloader".source = ./config;
}

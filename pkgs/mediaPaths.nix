rec {
  videoImage = "/mnt/video-image";
  # The flat list of independent videos, with the playlists in a subdirectory of it.
  youtubeVideos = "${videoImage}/Videos";
  youtubePlaylists = "${youtubeVideos}/playlists";
  variousVideos = "/run/media/andreas/various";
  jdownloaderJar = "/home/andreas/applications/jd2/JDownloader.jar";
}

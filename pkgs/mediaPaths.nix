rec {
  extSsd = "/run/media/andreas/20260414--ext-ssd";
  # The flat list of independent videos, with the playlists in a subdirectory of it.
  youtubeVideos = "${extSsd}/videos/youtube";
  youtubePlaylists = "${youtubeVideos}/playlists";
  variousVideos = "/run/media/andreas/various";
  jdownloaderJar = "/home/andreas/applications/jd2/JDownloader.jar";
}

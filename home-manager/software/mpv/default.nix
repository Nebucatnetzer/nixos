{ ... }:
{
  programs.mpv = {
    enable = true;
    bindings = {
      s = "playlist-shuffle";
    };
  };
  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "application/pdf" = ["org.gnome.Evince.desktop"];
      "inode/directory" = ["mpv.desktop"];
    };
  };
}

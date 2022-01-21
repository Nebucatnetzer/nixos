{ ... }:
{
  programs.mpv = {
    enable = true;
    bindings = {
      s = "playlist-shuffle";
    };
    config = {
      "keepaspect-window" = "no";
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

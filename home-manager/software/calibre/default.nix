{ pkgs, ... }:
{
  home.packages = with pkgs; [
    calibre
  ];

  xdg.mimeApps = {
    enable = true;
    associations.removed = {
      "application/pdf" = "calibre-ebook-viewer.desktop";
      "text/anytext" = "calibre-ebook-viewer.desktop";
    };
  };
}

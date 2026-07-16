{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.calibre ];

  xdg.mime.removedAssociations = {
    "application/pdf" = "calibre-ebook-viewer.desktop";
    "application/text" = "calibre-ebook-viewer.desktop";
    "application/textedit" = "calibre-ebook-viewer.desktop";
    "text/anytext" = "calibre-ebook-viewer.desktop";
    "text/plain" = "calibre-ebook-viewer.desktop";
  };
}

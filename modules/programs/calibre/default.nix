{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.calibre ];
  # The book files live on the archive disk (NFS). The SQLite DB stays local.
  environment.sessionVariables.CALIBRE_OVERRIDE_DATABASE_PATH = "/home/${config.az-username}/data/98_applications/calibre/metadata.db";

  xdg.mime.removedAssociations = {
    "application/pdf" = "calibre-ebook-viewer.desktop";
    "application/text" = "calibre-ebook-viewer.desktop";
    "application/textedit" = "calibre-ebook-viewer.desktop";
    "text/anytext" = "calibre-ebook-viewer.desktop";
    "text/plain" = "calibre-ebook-viewer.desktop";
  };
}

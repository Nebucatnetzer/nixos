{ pkgs, lib, ... }:
lib.meta.addMetaAttrs
  {
    description = "Download web articles for offline reading";
    license = lib.licenses.gpl3Plus;
    mainProgram = "download-articles";
    platforms = lib.platforms.linux;
  }
  (
    pkgs.writers.writePython3Bin "download-articles" {
      flakeIgnore = [ "E501" ];
    } (builtins.readFile ./download_articles.py)
  )

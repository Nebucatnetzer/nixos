{ pkgs, lib, ... }:
lib.meta.addMetaAttrs
  {
    description = "Rename files to follow the Denote naming convention";
    license = lib.licenses.gpl3Plus;
    mainProgram = "denote-rename";
    platforms = lib.platforms.linux;
  }
  (
    pkgs.writers.writePython3Bin "denote-rename" {
      flakeIgnore = [ "E501" ];
    } (builtins.readFile ./denote_rename.py)
  )

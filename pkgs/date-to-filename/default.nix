{ pkgs, lib, ... }:
lib.meta.addMetaAttrs
  {
    description = "Prepend the current date to a filename in ISO format";
    license = lib.licenses.gpl3Plus;
    mainProgram = "date-to-filename";
    platforms = lib.platforms.linux;
  }
  (
    pkgs.writers.writePython3Bin "date-to-filename" {
      flakeIgnore = [ "E501" ];
    } (builtins.readFile ./date_to_filename.py)
  )

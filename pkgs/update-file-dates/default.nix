{ pkgs, lib, ... }:
lib.meta.addMetaAttrs
  {
    description = "Update file modification timestamps from embedded date strings in filenames";
    license = lib.licenses.gpl3Plus;
    mainProgram = "update-file-dates";
    platforms = lib.platforms.linux;
  }
  (
    pkgs.writers.writePython3Bin "update-file-dates" {
      flakeIgnore = [ "E501" ];
    } (builtins.readFile ./update_file_dates.py)
  )

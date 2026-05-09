{ pkgs, lib, ... }:
lib.meta.addMetaAttrs
  {
    description = "Remove orphaned sidecar files left next to deleted media";
    license = lib.licenses.gpl3Plus;
    mainProgram = "sidecar-cleanup";
    platforms = lib.platforms.linux;
  }
  (
    pkgs.writers.writePython3Bin "sidecar-cleanup" {
      flakeIgnore = [ "E501" ];
    } (builtins.readFile ./sidecar_cleanup.py)
  )

{ pkgs, ... }:
pkgs.writers.writePython3Bin "date-to-filename" {
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./date_to_filename.py)

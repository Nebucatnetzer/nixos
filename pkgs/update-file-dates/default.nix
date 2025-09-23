{ pkgs, ... }:
pkgs.writers.writePython3Bin "update-file-dates" {
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./update_file_dates.py)

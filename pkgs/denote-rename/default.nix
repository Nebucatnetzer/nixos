{ pkgs, ... }:
pkgs.writers.writePython3Bin "denote-rename" {
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./denote_rename.py)

{ pkgs, ... }:
pkgs.writers.writePython3Bin "download-articles" {
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./download_articles.py)

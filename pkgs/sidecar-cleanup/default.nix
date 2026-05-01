{ pkgs, ... }:
pkgs.writers.writePython3Bin "sidecar-cleanup" {
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./sidecar_cleanup.py)

{ pkgs }:
pkgs.writers.writePython3Bin "raiffeisen-csv-cleanup" {
  # Cleans up Raiffeisen CSV files for Actual Budget import
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./raiffeisen_csv_cleanup.py)

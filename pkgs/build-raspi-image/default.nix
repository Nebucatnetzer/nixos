{
  git,
  nix,
  writers,
}:
writers.writePython3Bin "build-raspi-image" {
  flakeIgnore = [
    "E501" # black has line length 88
  ];
} (builtins.readFile ./build_raspi_image.py)

{
  lib,
  nixos-rebuild-ng,
  writers,
}:
writers.writePython3Bin "dry-build-all" {
  flakeIgnore = [ "E501" ];
  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [ nixos-rebuild-ng ])
  ];
} (builtins.readFile ./dry_build_all.py)

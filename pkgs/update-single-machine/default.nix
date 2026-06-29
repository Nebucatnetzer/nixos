{
  lib,
  nixos-rebuild-ng,
  writers,
}:
writers.writePython3Bin "update-single-machine" {
  flakeIgnore = [ "E501" ];
  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [ nixos-rebuild-ng ])
  ];
} (builtins.readFile ./update_single_machine.py)

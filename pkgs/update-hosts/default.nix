{
  lib,
  nixos-rebuild-ng,
  writers,
}:
writers.writePython3Bin "update-hosts" {
  flakeIgnore = [ "E501" ];
  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [ nixos-rebuild-ng ])
  ];
} (builtins.readFile ./update_hosts.py)

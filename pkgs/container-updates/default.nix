{
  lib,
  ripgrep,
  skopeo,
  writers,
}:
writers.writePython3Bin "container-updates" {
  flakeIgnore = [ "E501" ];
  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [
      ripgrep
      skopeo
    ])
  ];
} (builtins.readFile ./container_updates.py)

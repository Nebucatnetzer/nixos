{
  lib,
  nixos-rebuild-ng,
  writeShellApplication,
}:
writeShellApplication {
  name = "test-build";
  runtimeInputs = [ nixos-rebuild-ng ];
  meta = {
    description = "Dry-run build all hosts";
    license = lib.licenses.gpl3Plus;
    mainProgram = "test-build";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./test_build.sh;
}

{
  lib,
  art,
  python3Packages,
  symlinkJoin,
  writers,
}:
let
  launcher = writers.writePython3Bin "ART" {
    flakeIgnore = [ "E501" ];
    libraries = [ python3Packages.dbus-python ];
    makeWrapperArgs = [
      "--set"
      "ART_REAL_BINARY"
      (lib.getExe' art "ART")
    ];
  } (builtins.readFile ./art_dimmed.py);
in
symlinkJoin {
  name = "art-dimmed-${art.version}";
  # The launcher comes first, so its bin/ART replaces the one from art.
  paths = [
    launcher
    art
  ];
  meta = {
    description = "ART launcher that dims all displays to 30 % and inhibits KDE night light";
    license = lib.licenses.gpl3Plus;
    mainProgram = "ART";
    platforms = lib.platforms.linux;
  };
}

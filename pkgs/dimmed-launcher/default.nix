{
  lib,
  package,
  binaryName,
  python3Packages,
  symlinkJoin,
  writers,
}:
let
  launcher = writers.writePython3Bin binaryName {
    flakeIgnore = [ "E501" ];
    libraries = [ python3Packages.dbus-python ];
    makeWrapperArgs = [
      "--set"
      "DIMMED_LAUNCHER_BINARY"
      (lib.getExe' package binaryName)
    ];
  } (builtins.readFile ./dimmed_launcher.py);
in
symlinkJoin {
  name = "${package.pname}-dimmed-${package.version}";
  # The launcher comes first, so its bin/${binaryName} replaces the one from the package.
  paths = [
    launcher
    package
  ];
  # Some .desktop files call the absolute store path, which would skip the launcher.
  postBuild = ''
    for desktop_file in "$out"/share/applications/*.desktop; do
      original_file="$(readlink --canonicalize "$desktop_file")"
      rm "$desktop_file"
      substitute "$original_file" "$desktop_file" \
        --replace-quiet "${lib.getExe' package binaryName}" "$out/bin/${binaryName}"
    done
  '';
  meta = {
    description = "${binaryName} launcher that dims all displays to 30 % and inhibits KDE night light";
    license = lib.licenses.gpl3Plus;
    mainProgram = binaryName;
    platforms = lib.platforms.linux;
  };
}

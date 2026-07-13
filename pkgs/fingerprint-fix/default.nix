{
  lib,
  gobject-introspection,
  libfprint,
  gusb,
  json-glib,
  python3,
  writers,
}:
writers.writePython3Bin "fingerprint-fix" {
  libraries = [ python3.pkgs.pygobject3 ];
  flakeIgnore = [
    "E402"
    "E501"
  ];
  makeWrapperArgs = [
    "--prefix"
    "LD_LIBRARY_PATH"
    ":"
    (lib.makeLibraryPath [
      gobject-introspection
      libfprint
      gusb
      json-glib
    ])
    "--prefix"
    "GI_TYPELIB_PATH"
    ":"
    (lib.makeSearchPath "lib/girepository-1.0" [
      libfprint
      gusb.out
      json-glib
    ])
  ];
} (builtins.readFile ./fingerprint_fix.py)

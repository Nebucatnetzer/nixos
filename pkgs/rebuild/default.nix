{
  lib,
  nixos-rebuild-ng,
  writeShellApplication,
}:
writeShellApplication {
  name = "rebuild";
  runtimeInputs = [
    nixos-rebuild-ng
  ];
  meta = {
    description = "Rebuild and switch the local NixOS configuration";
    license = lib.licenses.gpl3Plus;
    mainProgram = "rebuild";
    platforms = lib.platforms.linux;
  };
  text = ''
    nixos-rebuild-ng -j auto switch --sudo
  '';
}

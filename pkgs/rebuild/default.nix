{
  nixos-rebuild-ng,
  writeShellApplication,
}:
writeShellApplication {
  name = "rebuild";
  runtimeInputs = [
    nixos-rebuild-ng
  ];
  text = ''
    nixos-rebuild-ng -j auto switch --sudo
  '';
}

{
  builderHost,
  lib,
  netcat,
  nixos-rebuild-ng,
  writeShellApplication,
}:
writeShellApplication {
  name = "rebuild";
  runtimeInputs = [
    netcat
    nixos-rebuild-ng
  ];
  meta = {
    description = "Rebuild and switch the local NixOS configuration";
    license = lib.licenses.gpl3Plus;
    mainProgram = "rebuild";
    platforms = lib.platforms.linux;
  };
  text = ''
    builders=()
    if nc -zw2 ${builderHost} 22 >/dev/null 2>&1; then
      echo "Builder ${builderHost} is reachable, offloading."
      builders=(--builders '@/etc/nix/machines')
    else
      echo "Builder ${builderHost} is unreachable, building locally."
    fi

    nixos-rebuild -j auto switch --sudo "''${builders[@]}"
  '';
}

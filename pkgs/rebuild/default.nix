{
  builderHost,
  iproute2,
  lib,
  netcat,
  nixos-rebuild-ng,
  writeShellApplication,
}:
writeShellApplication {
  name = "rebuild";
  runtimeInputs = [
    iproute2
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
    # Check if we are running on the remote builder itself
    if ip -oneline address show | grep --quiet --fixed-strings " ${builderHost}/"; then
      echo "This host is the builder ${builderHost}, building locally."
    elif nc -zw2 ${builderHost} 22 >/dev/null 2>&1; then
      echo "Builder ${builderHost} is reachable, offloading."
      builders=(--builders '@/etc/nix/machines')
    else
      echo "Builder ${builderHost} is unreachable, building locally."
    fi

    nixos-rebuild -j auto switch --sudo "''${builders[@]}"
  '';
}

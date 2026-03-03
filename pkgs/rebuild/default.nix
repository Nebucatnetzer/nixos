{
  netcat,
  nixos-rebuild-ng,
  writeShellApplication,
}:
writeShellApplication {
  name = "rebuild";
  runtimeInputs = [
    nixos-rebuild-ng
  ];
  text = ''
    if ${netcat}/bin/nc -vzw 2 cache.zweili.org 2222; then
      nixos-rebuild-ng -j auto switch --sudo
      upload-to-cache /run/current-system
    else
      echo "Build without private cache"
      sudo nixos-rebuild-ng switch --option substituters "https://cache.nixos.org https://nix-community.cachix.org"
    fi
  '';
}

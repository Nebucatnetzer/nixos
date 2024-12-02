{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "nixosbuildshell";
  nativeBuildInputs = [ ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nix}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
  '';
}

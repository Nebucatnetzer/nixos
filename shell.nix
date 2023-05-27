{ pkgs ? import <nixpkgs> { } }:
let
  nix-refactor = pkgs.writeShellScriptBin "nix-refactor" ''
    origin=$(${pkgs.coreutils-full}/bin/readlink /run/current-system)
    result=$(${pkgs.lib.getExe pkgs.nixos-rebuild} build && ${pkgs.coreutils-full}/bin/readlink result)
    ${pkgs.diffutils}/bin/diff -q <(echo "$origin" ) <(echo "$result")
  '';
in
pkgs.mkShell {
  name = "nixosbuildshell";
  nativeBuildInputs = with pkgs; [
    git
    nixFlakes
    nix-refactor
    python3
    python310Packages.autopep8
    python310Packages.black
    python310Packages.flake8
    python310Packages.jedi
    python310Packages.pip
    python310Packages.yapf
  ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
  '';
}


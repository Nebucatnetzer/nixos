{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "nixosbuildshell";
  nativeBuildInputs = with pkgs; [
    git
    nixFlakes
    python3
    python39Packages.autopep8
    python39Packages.black
    python39Packages.flake8
    python39Packages.jedi
    python39Packages.pip
    python39Packages.yapf
  ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
  '';
}

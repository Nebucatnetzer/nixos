{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "nixosbuildshell";
  nativeBuildInputs = [
    pkgs.deadnix
    pkgs.git
    pkgs.python3
    pkgs.python3Packages.black
    pkgs.python3Packages.mypy
    pkgs.python3Packages.pylint
    pkgs.qtile-unwrapped
    pkgs.python3Packages.python-lsp-server
    pkgs.vim
  ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nix}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
    export DEVENV_ROOT=$(pwd)
  '';
}

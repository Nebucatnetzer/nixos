{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "nixosbuildshell";
  nativeBuildInputs = with pkgs; [
    git
    nixFlakes
    python3
    python3Packages.black
    python3Packages.mypy
    python3Packages.pylint
    qtile
    python3Packages.python-lsp-server
    vim
  ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
    export DEVENV_ROOT=$(pwd)
  '';
}

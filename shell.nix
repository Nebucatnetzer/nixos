{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "nixosbuildshell";
  packages = [
    (pkgs.buildEnv {
      name = "zweili-nixos-devShell";
      paths = [
        pkgs.deadnix
        pkgs.git
        pkgs.python3
        pkgs.black
        pkgs.mypy
        pkgs.pylint
        pkgs.python3Packages.python-lsp-server
        pkgs.vim
      ];
      pathsToLink = [ "/bin" ];
    })
  ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nix}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
    export DEVENV_ROOT=$(pwd)
  '';
}

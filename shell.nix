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
        (pkgs.python3.withPackages (p: [
          p.python-lsp-server
          p.python-lsp-ruff
          p.black
          p.docformatter
          p.isort
          p.mypy
          p.ruff
          p.pylint
        ]))
        pkgs.vim
      ];
      pathsToLink = [ "/bin" ];
    })
  ];

  shellHook = ''
    export DEVENV_ROOT=$(pwd)
  '';
}

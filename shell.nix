{
  buildEnv,
  deadnix,
  git,
  pkgs,
  python3,
  mkShellNoCC,
  unstable-pkgs,
  vim,
  wireguard-tools,
}:
let
  azPkgs = import ./pkgs { inherit pkgs unstable-pkgs; };
in
mkShellNoCC {
  name = "nixosbuildshell";
  packages = [
    (buildEnv {
      name = "zweili-nixos-devShell";
      paths = [
        azPkgs.update-single-machine
        deadnix
        git
        wireguard-tools
        (python3.withPackages (p: [
          p.python-lsp-server
          p.python-lsp-ruff
          p.black
          p.docformatter
          p.isort
          p.mypy
          p.ruff
          p.pylint
        ]))
        vim
      ];
      pathsToLink = [
        "/bin"
        "/share"
      ];
    })
  ];

  shellHook = ''
    export DEVENV_ROOT=$(pwd)
  '';
}

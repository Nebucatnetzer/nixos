{
  buildEnv,
  deadnix,
  git,
  python3,
  mkShellNoCC,
  vim,
  wireguard-tools,
}:
mkShellNoCC {
  name = "nixosbuildshell";
  packages = [
    (buildEnv {
      name = "zweili-nixos-devShell";
      paths = [
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

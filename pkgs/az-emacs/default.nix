{
  pkgs,
  unstable-pkgs,
  lib,
  writeShellApplication,
  emacs-nox,
  runCommand,
  ...
}:
let
  emacsConfig = runCommand "az-emacs-config" { } ''
    cp -r ${../../modules/home-manager/programs/emacs/emacs.d} $out
    chmod -R u+w $out
    substituteInPlace $out/init.el \
      --replace-fail '~/.nixos/modules/home-manager/programs/emacs/emacs.d/lib/' '${placeholder "out"}/lib/' \
      --replace-fail '~/.nixos/modules/home-manager/programs/emacs/emacs.d/modules.el' '${placeholder "out"}/modules.el'
  '';

  emacsWithPkgs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (
    epkgs:
    import ../../modules/home-manager/programs/emacs/extra-packages.nix {
      inherit
        pkgs
        unstable-pkgs
        epkgs
        lib
        ;
      includeGuiPackages = false;
    }
  );
in
writeShellApplication {
  name = "az-emacs";
  runtimeInputs = [
    emacsWithPkgs
    pkgs.fd
    pkgs.ripgrep
    pkgs.pandoc
    pkgs.mu
    pkgs.scowl
  ];
  meta = {
    description = "Andreas's terminal Emacs setup, bundled for portable nix run";
    mainProgram = "az-emacs";
    platforms = lib.platforms.linux;
  };
  text = ''
    export EMACS_DICT_WORDS="${pkgs.scowl}/share/dict/wbritish.txt"
    export LSP_USE_PLISTS=true
    exec ${emacsWithPkgs}/bin/emacs --init-directory ${emacsConfig} "$@"
  '';
}

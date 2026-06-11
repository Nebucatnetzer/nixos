{
  pkgs,
  unstable-pkgs,
  lib,
  emacsDir,
  emacsPackagesFor,
  emacs-nox,
  writeShellApplication,
  runCommand,
  fd,
  ripgrep,
  pandoc,
  scowl,
}:
let
  emacsConfig = runCommand "az-emacs-config" { } ''
    cp -r ${emacsDir}/emacs.d $out
    chmod -R u+w $out
    # Bundle a terminal-only feature-flag file so the nox build does not pick
    # up the GUI variables.el from ~/.emacs.d (which would enable scroll bars,
    # pdf-tools, fonts, ... that don't exist in a no-X Emacs).
    cp ${./nox-variables.el} $out/variables.el
    substituteInPlace $out/init.el \
      --replace-fail '~/.nixos/modules/home-manager/programs/emacs/emacs.d/lib/' '${placeholder "out"}/lib/' \
      --replace-fail '~/.nixos/modules/home-manager/programs/emacs/emacs.d/modules.el' '${placeholder "out"}/modules.el' \
      --replace-fail '~/.emacs.d/variables.el' '${placeholder "out"}/variables.el'
    # Disable use-package-always-ensure since all packages are pre-installed via Nix
    substituteInPlace $out/init.el \
      --replace-fail '(setq use-package-always-ensure t)' '(setq use-package-always-ensure nil)'
  '';

  emacsWithPkgs = (emacsPackagesFor emacs-nox).emacsWithPackages (
    epkgs:
    import (emacsDir + "/extra-packages.nix") {
      inherit
        pkgs
        unstable-pkgs
        epkgs
        lib
        ;
      includeGuiPackages = false;
      includeExtendedPackages = false;
    }
  );
in
writeShellApplication {
  name = "az-emacs";
  runtimeInputs = [
    emacsWithPkgs
    fd
    ripgrep
    pandoc
  ];
  meta = {
    description = "Andreas's terminal Emacs setup, bundled for portable nix run";
    mainProgram = "az-emacs";
    platforms = lib.platforms.linux;
  };
  text = ''
    export EMACS_DICT_WORDS="${scowl}/share/dict/wbritish.txt"
    export LSP_USE_PLISTS=true
    exec ${emacsWithPkgs}/bin/emacs --init-directory ${emacsConfig} "$@"
  '';
}

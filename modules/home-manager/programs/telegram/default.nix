{ inputs, pkgs, ... }:
let
  telegram =
    inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.telegram-desktop;
in
{
  home.packages = [ telegram ];
}

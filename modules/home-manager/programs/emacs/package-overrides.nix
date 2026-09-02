# ELPA serves the current release as org-<v>.tar and archives older ones as
# org-<v>.tar.lz, but its archive skips 9.8.3 entirely, so the version nixpkgs
# pins is unfetchable. Pin a release that is actually served; the .lz fallback
# (same trick as nixpkgs' own fetchelpa.nix) keeps the hash valid once 9.8.10
# stops being the current release.
{ pkgs }:
let
  version = "9.8.10";
  base = "https://elpa.gnu.org/packages/org-${version}.tar";
in
_: prev: {
  org = prev.org.overrideAttrs (_: {
    inherit version;
    src = pkgs.fetchurl {
      name = "org-${version}.tar";
      urls = [
        base
        "${base}.lz"
      ];
      hash = "sha256-iQqd1cTx0nn27Qs9EGcNeWqsy3/ccaKmZc0VO+xGPx0=";
      postFetch = ''
        if [[ $url == *.lz ]]; then
          ${pkgs.lzip}/bin/lzip -c -d $out > uncompressed
          mv uncompressed $out
        fi
      '';
    };
  });
}

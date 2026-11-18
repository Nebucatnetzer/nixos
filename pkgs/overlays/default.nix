[
  # TODO: Remove once the nixos-26.05 channel ships nginx >= 1.30.7.
  # Fixes CVE-2026-123007, CVE-2026-123633, CVE-2026-121810, CVE-2026-118899 and others.
  (final: prev: {
    nginxStable = prev.nginxStable.overrideAttrs (old: {
      version = "1.30.7";
      src = final.fetchurl {
        url = "https://nginx.org/download/nginx-1.30.7.tar.gz";
        hash = "sha256-JWW1yDcFDpWPcPsPc8SOK8pFQq00n9NW9G+wxyYQvG4=";
      };
    });
  })
]

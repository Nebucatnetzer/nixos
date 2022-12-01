#!/usr/bin/env bash
set -e

hosts=($(echo `nix eval .#nixosConfigurations --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'` | xargs ))

for host in "${hosts[@]}"
do
    echo $host
    nix build -j auto --dry-run .#nixosConfigurations.${host}.config.system.build.toplevel
    echo
    echo
done

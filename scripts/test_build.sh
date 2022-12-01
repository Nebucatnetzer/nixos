#!/usr/bin/env bash
set -e

hosts=($(echo `nix eval .#nixosConfigurations --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'` | xargs ))

for host in "${hosts[@]}"
do
    echo $host
    nixos-rebuild switch -j auto dry-build --flake ".#$host"
    echo
    echo
done

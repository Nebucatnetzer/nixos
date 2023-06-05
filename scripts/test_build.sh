#!/usr/bin/env bash
set -e

hosts=($(echo `nix eval .#nixosConfigurations --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'` | xargs ))
skip=(
    "desktop-vm"
    "gwyn"
    "loki-test"
    "test-raspi"
    "staubfinger"
)


for host in "${hosts[@]}"
do
    if [[ " ${skip[*]} " =~ " ${host} " ]];then
        continue
    fi
    echo $host
    nixos-rebuild dry-build --flake .#${host}
    echo
    echo
done

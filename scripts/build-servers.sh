#!/usr/bin/env bash

hosts=($(echo `nix eval .#nixosConfigurations --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'` | xargs ))
skip=(
    "desktop-vm"
    "gwyn"
    "staubfinger"
)

for host in "${hosts[@]}"
do
    # Check if the host is in the skip list
    if [[ " ${skip[*]} " =~ " ${host} " ]];then
        continue
    fi
    nix build .#nixosConfigurations.${host}.config.system.build.toplevel
done

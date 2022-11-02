#!/usr/bin/env bash

hosts=($(echo `nix eval .#nixosConfigurations --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'` | xargs ))
skip=(
    "gwyn"
    "loki-test"
    "desktop-vm"
)

rsa_key="$HOME/.nixos/secrets/ssh_keys/ansible/ansible.key"
export NIX_SSHOPTS="-t -i $rsa_key"

for host in "${hosts[@]}"
do
    # Check if the host is in the skip list
    if [[ " ${skip[*]} " =~ " ${host} " ]];then
        continue
    fi
    fqdn="$host.2li.local"
    echo $fqdn
    nixos-rebuild switch -j auto --use-remote-sudo --build-host localhost --target-host $fqdn --flake ".#$host"
    echo "reboot $fqdn"
    ssh -i $rsa_key $fqdn 'sudo reboot'
    echo
    echo
done

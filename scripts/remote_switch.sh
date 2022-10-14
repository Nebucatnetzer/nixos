#!/usr/bin/env bash

hosts=(
 "mail"
 "nextcloud"
 "plex"
 "management"
 "restic-server"
 "ttrss"
 "git"
 "proxy"
 "pihole"
)

rsa_key="$HOME/.nixos/secrets/ssh_keys/ansible/ansible.key"
export NIX_SSHOPTS="-t -i $rsa_key"

for host in "${hosts[@]}"
do
    fqdn="$host.2li.local"
    echo $fqdn
    nixos-rebuild switch -j auto --use-remote-sudo --build-host localhost --target-host $fqdn --flake ".#$host"
    echo "reboot $fqdn"
    ssh -i $rsa_key $fqdn 'sudo reboot'
    echo
    echo
done

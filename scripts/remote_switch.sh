#!/usr/bin/env bash

hosts=(
 "nixos-management"
 #"grav"
 "git"
 #"heimdall"
 "jdownloader"
 #"k3s-master1"
 #"k3s-node1"
 #"k3s-node2"
 "mail"
 "nextcloud"
 #"nomad-master1"
 #"nomad-client1"
 "pihole"
 "plex"
 #"proxy"
 "restic-server"
 #"rss-bridge"
 #"test-server"
 "ttrss"
)

rsa_key="$HOME/.nixos/secrets/ssh_keys/ansible/ansible.key"
export NIX_SSHOPTS="-t -i $rsa_key"

for host in "${hosts[@]}"
do
    fqdn="$host.2li.local"
    echo $fqdn
    nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host $fqdn --flake ".#$host"
    echo
    echo
done
for host in "${hosts[@]}"
do
    fqdn="$host.2li.local"
    if [ $host == "nixos-management" ]; then
        continue
    fi
    echo "reboot $fqdn"
    ssh -i rsa_key 'sudo reboot'
    echo
    echo
done

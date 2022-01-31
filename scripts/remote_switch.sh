#!/usr/bin/env bash

hosts = (
 "nixos-management"
 "grav"
 "git"
 "heimdall"
 "mail"
 "plex"
 "proxy"
 "rss-bridge"
 "ttrss"
)

rsa_key="~/.nixos/secrets/ssh_keys/ansible/ansible.key"
$NIX_SSHOPTS="-t -i $rsa_key"

for host in $hosts do
    fqdn="$host.2li.local"
    echo $fqdn
    nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host $fqdn --flake ".#$host"
    echo
    echo
done

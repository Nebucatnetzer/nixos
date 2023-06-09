#!/usr/bin/env bash

rsa_key="~/.nixos/secrets/ssh_keys/ansible/ansible.key"
export NIX_SSHOPTS="-t -i $rsa_key"

host=$1
fqdn="$host.2li.local"
nixos-rebuild switch --use-remote-sudo --target-host $fqdn --flake ".#$host"

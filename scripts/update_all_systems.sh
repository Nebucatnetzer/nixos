#!/usr/bin/env bash

hosts = (
 "nixos-management"
 "grav"
 "heimdall"
 "mail"
 "plex"
 "proxy"
 "rss-bridge"
 "ttrss"
)

update_command='tmux new -s updates "cd ~/.nixos && git pull && sudo nixos-rebuild -j auto switch || bash;"'
rsa_key="~/.nixos/secrets/ssh_keys/ansible/ansible.key"

for host in $hosts do
    fqdn="$host.2li.local"
    echo $fqdn
    ssh -i $rsa_key -t $fqdn $update_command
    echo
    echo
done

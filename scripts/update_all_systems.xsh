#!/usr/bin/env xonsh

hosts = [
 "nixos-management",
 "grav",
 "heimdall",
 "mail",
 "plex",
 "proxy",
 "rss-bridge",
 "ttrss",
]

update_command = 'tmux new -s updates "cd ~/.nixos && git pull && sudo nixos-rebuild -j auto switch || bash;"'
rsa_key = "~/.nixos/secrets/ssh_keys/ansible/ansible.key"

for host in hosts:
    fqdn = "{}.2li.local".format(host)
    print(fqdn)
    print("-" * len(fqdn))
    ssh -i @(rsa_key) -t @(fqdn) @(update_command)
    print("")
    print("")

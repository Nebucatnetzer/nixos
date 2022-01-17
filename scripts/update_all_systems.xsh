#!/usr/bin/env xonsh

hosts = [
 "grav",
 "heimdall",
 "nixos-test-vm",
]

for host in hosts:
    fqdn = "{}.2li.local".format(host)
    print(fqdn)
    print("-" * len(fqdn))
    ssh -t @(fqdn) 'tmux new -s updates "cd ~/.nixos && git pull && sudo nixos-rebuild -j auto switch || bash;"'
    print("")
    print("")

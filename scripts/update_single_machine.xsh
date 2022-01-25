#!/usr/bin/env xonsh

rsa_key = "~/.nixos/secrets/ssh_keys/ansible/ansible.key"
$NIX_SSHOPTS="-t -i " + rsa_key

host = input("Please enter the host to deploy: ")
fqdn = "{}.2li.local".format(host)
print(fqdn)
print("-" * len(fqdn))
nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host @(fqdn) --flake @(".#" + host)
#echo @(fqdn) @(".#" + host)
print("")
print("")

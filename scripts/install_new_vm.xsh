#!/usr/bin/env xonsh

host = "nixos"

download_command = "curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz"
nixshell_command = "cd nixos && nix-shell"
format_command = "sudo python3 scripts/format-disk.py"
install_command = "sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#" + host

for host in hosts:
    fqdn = "{}.2li.local".format(host)
    ssh-copy-id @(fqdn)
    ssh -t @(fqdn) @(download_command)
    ssh -t @(fqdn) @(nixshell_command)
    ssh -t @(fqdn) @(format_command)
    ssh -t @(fqdn) @(install_command)

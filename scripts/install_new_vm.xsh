#!/usr/bin/env xonsh

flake = input("Enter a config you want to deploy: ")

server = "nixos@nixos.2li.local"
download_command = "curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz"
nixshell_command = "cd nixos && nix-shell"
format_command = "sudo python3 scripts/format-disk.py"
install_command = "sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#" + flake

ssh-copy-id @(server)
ssh -t @(server) @(download_command)
ssh -t @(server) @(nixshell_command)
ssh -t @(server) @(format_command)
ssh -t @(server) @(install_command)

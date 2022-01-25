#!/usr/bin/env xonsh

flake = input("Enter a config you want to deploy: ")

server = "nixos@nixos.2li.local"
download_command = "curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz"
install_command = "cd ~/nixos && ./scripts/install_vm.sh " + flake
rsa_key = "~/.ssh/id_rsa"

ssh-copy-id @(server)
ssh -i @(rsa_key) -t @(server) @(download_command)
ssh -i @(rsa_key) -t @(server) @(install_command)

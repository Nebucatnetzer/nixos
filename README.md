# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Base installation

1. `nix-shell`
2. `sudo python3 scripts/format-disk.py`
4. `sudo nixos-install --flake ~/nixos#SYSTEMNAME`

## Non-Nixos System

1. `scripts/add-home-manager-channel.sh`
2. `scripts/install-home-manager.sh`

# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Base installation

1. `sudo python3 scripts/format-disk.py`
2. Execute one of the following scripts:
    - `sudo scripts/link-asus.sh`
    - `sudo scripts/link-vm.sh`
    - `sudo scripts/link-precision5530.sh`
3. `sudo nixos-install`

## Non-Nixos System

1. `scripts/add-home-manager-channel.sh`
2. `scripts/install-home-manager.sh`

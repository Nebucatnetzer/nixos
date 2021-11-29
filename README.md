# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Base installation

```
parted /dev/sda -- mklabel gpt
parted /dev/sda -- mkpart ESP fat32 1MiB 512MiB
parted /dev/sda -- set 1 esp on
parted /dev/sda -- mkpart primary 512MiB 100%

cryptsetup luksFormat --type luks1 /dev/sda2
cryptsetup open /dev/sda2 cryptlvm

pvcreate /dev/mapper/cryptlvm
vgcreate MainGroup /dev/mapper/cryptlvm
lvcreate -L 8G MainGroup -n swap
lvcreate -l 100%FREE MainGroup -n root

mkfs.ext4 -L nixos /dev/MainGroup/root
mkswap -L swap /dev/MainGroup/swap
mkfs.fat -F 32 -n BOOT /dev/sda1
```

1. `sudo scripts/format-drive.sh`
2. Execute one of the following scripts:
    - `sudo scripts/link-asus.sh`
    - `sudo scripts/link-vm.sh`
    - `sudo scripts/link-precision5530.sh`
3. `sudo nixos-install`

## Non-Nixos System

1. `scripts/add-home-manager-channel.sh`
2. `scripts/install-home-manager.sh`

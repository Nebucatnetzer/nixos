# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## VM installation

This allows you to setup a VM with minimal effort.
However this requires some prerequisites.

- A VM running the minimal NixOS ISO reachable with SSH under nixos@nixos.2li.local.
- The VM runs UEFI.
- You've set a password for the nixos user.

1. `./scripts/install_new_vm.xsh`
2. Provide the required information and wait until the script is finished.
3. Reboot and profit!

## Raspberry Pi installation

1. Download the image from: https://hydra.nixos.org/job/nixos/trunk-combined/nixos.sd_image.aarch64-linux
2. Extract it with: `unzstd nixos-sd-image-*-aarch64-linux.img.zst`
3. Flash it to the SD card with `sudo dd if=$(ls nixos-sd-image-*-aarch64-linux.img) of=/dev/mmcblk0 bs=4M`
4. `curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz`
5. `sudo cp nixos/systems/raspi4/init_config.nix /etc/nixos/configuration.nix`
6. `sudo nixos-rebuild switch`

## x86 installation

1. `curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz`
2. `cd nixos && nix-shell`
3. `sudo python3 scripts/format-disk.py`
4. `sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#SYSTEMNAME`

## Update remote systems

Simply run the script `scripts/update_all_systems.xsh` and it will iterate over
all defined systems.
The script requires Xonsh but can be easily adapted to BASH or similar.

## Non-Nixos System

1. `scripts/install-home-manager.sh`

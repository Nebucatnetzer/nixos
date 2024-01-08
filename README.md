# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Preparation

On a PC you don't have to do anything special.

For a Raspberry Pi you need to prepare the SD card first with a UEFI partition. On a PC navigate into this project and run the following commands:

- `nix-shell`
- `sudo create-uefi-partition.sh`

This will format the SD card at `/dev/mmcblk0`, create a partition and download and copy all the required files for running UEFI on a Pi 4.

## Installation

1. Insert an USB stick with the latest NixOS ISO into your device.
1. `curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz`
1. `cd nixos && nix-shell setup-shell.nix`
1. For a normal PC run: `sudo ./scripts/format-disk.py` on a Raspberry Pi 4 run: `sudo ./scripts/format-sdcard.py`
1. `sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#SYSTEMNAME`

When everything is finished you can reboot the system and remove the USB stick. You have now a fully encrypted NixOS system.

## Update remote systems

Simply run the script `scripts/remote_switch.sh` and it will iterate over
all defined systems. With the option `-r` the systems will reboot as well.

## Non-Nixos System

1. `scripts/install-home-manager.sh`

## Development

### Options template

```nix
{ config, lib, pkgs, ... }:
let
  cfg = config.programs.NAME;
in
{
  options = {
    programs.NAME.enable = lib.mkEnableOption "DESCRIPTION";
  };

  config = lib.mkIf cfg.enable {
  };
}
```

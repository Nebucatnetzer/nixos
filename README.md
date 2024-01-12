# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Installation

1. For Raspis it's the easiest if you prepare the SD card/disk on another system. For a PC you can just boot the installation ISO directly.
1. For both devices you can format the disk/card with the following script `sudo ./scripts/format-disk.py`. It will walk you through the formatting process and for a Raspi4 it will prepare it for UEFI setup.
1. Next install the system with `sudo nixos-install --no-root-passwd --root /mnt/nixos --impure --flake .#SYSTEMNAME`

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

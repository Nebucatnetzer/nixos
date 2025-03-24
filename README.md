# nixos

This repository lives on https://git.2li.ch/.
Due to agressive LLM crawlers I had to make it private there.
If you want to collaborate you can contact me through email or Mastodon.

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Installation

1. For Raspis it's the easiest if you prepare the SD card/disk on another system. For a PC you can just boot the installation ISO directly.
2. For both devices you can format the disk/card with the following script `sudo ./scripts/format-disk.sh`. Make sure to edit it beforehand, to point it to the correct disk.
3. Next install the system with `sudo nixos-install --no-root-passwd --root /mnt/nixos --impure --flake .#SYSTEMNAME --no-channel-copy`
4. Rename the partitions with the script `sudo ./scripts/rename-partitions.sh`. With this script as well. Check that you're pointing to the correct disk.

When everything is finished you can reboot the system and remove the USB stick. You have now a fully encrypted NixOS system.

### Additional script

- If you only want to prepare an SD card with an UEFI partition for a Raspberry Pi 4 you can use the script `sudo ./scripts/create-uefi.sh`

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

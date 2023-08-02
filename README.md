# nixos

This repository contains my configuration for my Nixos systems.
I don't provide any garantuees that it will work on other systems.
In addition some of the scripts required for installation will destroy your data when used.

## Raspberry Pi installation

1. Add the new system to `flake.nix`.
2. Build the image with `scripts/build-raspi-image.sh SYSTEMNAME`
3. Flash the image to an SD card
   `dd if=~/Downloads/SYSTEMNAME.img of=/dev/mmcblk0 bs=4M`.
4. After you've booted the Pi get the new SSH key with
   `ssh-keyscan hostname.custom.domain` and add it to `scrts/secrets.nix`.
5. Then login into the new Pi and mount the `FIRMWARE` partition with
   `sudo mkdir -p /mnt && sudo mount /dev/disk/by-label/FIRMWARE /mnt` and make
   sure that your `config.txt` looks like [./systems/raspi4/config.txt](./systems/raspi4/config.txt)
6. Change the password

## x86 installation

1. `curl https://git.2li.ch/Nebucatnetzer/nixos/archive/master.tar.gz | tar xz`
2. `cd nixos && nix-shell`
3. `sudo ./scripts/format-disk.py`
4. `sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#SYSTEMNAME`

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

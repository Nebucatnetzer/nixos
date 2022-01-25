#!/usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

sudo python3 scripts/format-disk.py
sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#$1

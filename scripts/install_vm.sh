#!/usr/bin/env bash

cd nixos && nix-shell
sudo python3 scripts/format-disk.py
sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#$1

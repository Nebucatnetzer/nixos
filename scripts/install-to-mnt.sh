#!/usr/bin/env bash

nixos-install --no-root-passwd --root /mnt/nixos --impure --flake .#$1

#!/usr/bin/env bash
nix-shell -p python3
sudo python3 scripts/format-disk.py

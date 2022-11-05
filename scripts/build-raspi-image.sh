#!/usr/bin/env bash

host=$1

cd ~/.nixos/

nix build .#images.$host &&
unzstd result/sd-image/*.img.zst -o ~/Downloads/$host.img

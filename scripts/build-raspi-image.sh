#!/usr/bin/env bash

host=$1
image=~/Downloads/$host.img

cd ~/.nixos/

nix build .#images.$host &&
unzstd result/sd-image/*.img.zst -o $image
touch $image
chmod 644 $image

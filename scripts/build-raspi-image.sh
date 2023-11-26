#!/usr/bin/env bash

host=$1
image=~/Downloads/$host.img.zst

cd ~/.nixos/

nom build .#images.$host &&
    cp result/sd-image/*.img.zst $image
chmod 644 $image

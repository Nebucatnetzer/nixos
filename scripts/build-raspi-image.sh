#!/usr/bin/env bash

host=$1
image=~/Downloads/$host.img


nix build .#images.$host &&
cp result/sd-image/*.img $image

#! /usr/bin/env bash

set -e

# Fail if $SUDO_USER is empty.
if [ -z "$SUDO_USER" ]; then
    printf "This script must be run with sudo.\n"
    exit 1
fi

DISK=/dev/nvme0n1

BOOT_PARTITION="$DISK"p1
ROOT_PARTITION="$DISK"p2
ROOT_DIR=/mnt/nixos
BOOT_DIR=/mnt/nixos/boot
LUKS_NAME=crypttoformat
LUKS_PATH=/dev/mapper/$LUKS_NAME

mount_partitions() {
    echo "Mount partitions."
    cryptsetup open $ROOT_PARTITION $LUKS_NAME
    sleep 5
    mount -o subvol=root,compress=zstd,noatime "$LUKS_PATH" "$ROOT_DIR"
    mkdir -p "$ROOT_DIR"/{home,nix,swap}
    mount -o subvol=home,compress=zstd,noatime "$LUKS_PATH" "$ROOT_DIR"/home
    mount -o subvol=nix,compress=zstd,noatime "$LUKS_PATH" "$ROOT_DIR"/nix
    mount -o subvol=swap,noatime "$LUKS_PATH" "$ROOT_DIR"/swap

    mkdir -p "$BOOT_DIR"
    mount /dev/disk/by-label/BOOT "$BOOT_DIR"
}

mount_partitions

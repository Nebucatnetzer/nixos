#! /usr/bin/env bash

set -e

# Fail if $SUDO_USER is empty.
if [ -z "$SUDO_USER" ]; then
    printf "This script must be run with sudo.\n"
    exit 1
fi

ROOT_DIR=/mnt/nixos
BOOT_DIR=/mnt/nixos/boot
LUKS_NAME=crypttoformat
LUKS_PATH=/dev/mapper/$LUKS_NAME

umount_partitions() {
    echo "Unmount partitions."
    sleep 5
    umount "$BOOT_DIR"
    umount "$ROOT_DIR"/home
    umount "$ROOT_DIR"/nix
    umount "$ROOT_DIR"/swap
    umount "$ROOT_DIR"
    cryptsetup close $LUKS_NAME
}

umount_partitions

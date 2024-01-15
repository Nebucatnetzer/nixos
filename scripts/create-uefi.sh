#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted

DISK=/dev/mmcblk0
set -e

BOOT_DIR=/mnt/nixos/boot

echo "Create partition table."
parted --script $DISK mklabel gpt
parted --script $DISK mkpart ESP fat32 0% 1GiB
parted --script $DISK set 1 esp on
sleep 5

echo "Create boot partition."
mkfs.fat -F32 -n BOOTTOFRMT "$DISK"p1
sleep 5

echo "Mount partitions."
mkdir -p $BOOT_DIR
mount /dev/disk/by-label/BOOTTOFRMT $BOOT_DIR

echo "Create UEFI"
curl -o /tmp/pi4-uefi.zip -L https://github.com/pftf/RPi4/releases/download/v1.35/RPi4_UEFI_Firmware_v1.35.zip
unzip /tmp/pi4-uefi.zip -d $BOOT_DIR
sync

umount $BOOT_DIR

echo "Rename boot partition."
fatlabel /dev/disk/by-label/BOOTTOFRMT BOOT

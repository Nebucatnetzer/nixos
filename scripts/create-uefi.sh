#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted unzip curl

# Create the boot partition
parted --script /dev/mmcblk0 mklabel gpt
parted --script /dev/mmcblk0 mkpart ESP fat32 0% 1GiB
parted --script /dev/mmcblk0 set 1 esp on
mkfs.fat -F32 -n SdBoot /dev/mmcblk0p1

# Download and install the UEFI firmware
mkdir -p /tmp/sdcard/boot
mount /dev/mmcblk0p1 /tmp/sdcard/boot
curl -o /tmp/pi4-uefi.zip -L https://github.com/pftf/RPi4/releases/download/v1.35/RPi4_UEFI_Firmware_v1.35.zip
unzip /tmp/pi4-uefi.zip -d /tmp/sdcard/boot
sync

# Cleanup
umount /tmp/sdcard/boot
rm /tmp/pi4-uefi.zip

#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted

set -e

# Fail if $SUDO_USER is empty.
if [ -z "$SUDO_USER" ]; then
    printf "This script must be run with sudo.\n"
    exit 1
fi

rename_boot_partition() {
    echo "Rename boot partition."
    fatlabel /dev/disk/by-label/BOOTTOFRMT BOOT
}

rename_btrfs() {
    echo "Rename btrfs partition."
    btrfs filesystem label /dev/nvme0n1 mainBtrfs
}

rename_f2fs() {
    echo "Rename f2fs partition."
    f2fslabel /dev/disk/by-label/ROOTTOFRMT root
}

unmount_partitions() {
    echo "Unmounting partitions."
    umount /mnt/nixos/boot
    umount /mnt/nixos/home
    umount /mnt/nixos/nix
    umount /mnt/nixos/swap
    umount /mnt/nixos
    sleep 3
}

close_luks() {
    cryptsetup close crypttoformat
}

rename_pc() {
    rename_btrfs
}

rename_raspi() {
    rename_f2fs
}

unmount_partitions
sleep 5
rename_boot_partition
# rename_raspi
rename_pc
close_luks

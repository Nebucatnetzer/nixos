#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted

rename_boot_partition() {
    echo "Rename boot partition."
    fatlabel /dev/disk/by-label/BOOTTOFRMT BOOT
}

rename_ext4() {
    echo "Rename ext4 partition."
    e2label /dev/MainGroup/roottoformat root
}

rename_f2fs() {
    echo "Rename f2fs partition."
    f2fslabel /dev/disk/by-label/ROOTTOFRMT root
}

rename_swap() {
    echo "Rename swap partition."
    swaplabel -L swap /dev/GroupToFormat/swaptoformat
}

rename_lvm() {
    echo "Rename LVM"
    lvrename GroupToFormat roottoformat root
    vgrename GroupToFormat MainGroup
}

unmount_partitions() {
    echo "Unmounting partitions."
    umount /mnt/nixos/boot
    umount /mnt/nixos
    sleep 3
}

close_luks() {
    cryptsetup close crypttoformat
}

rename_pc() {
    rename_ext4
    rename_lvm
    rename_swap
}

rename_raspi() {
    rename_f2fs
}

unmount_partitions
sleep 5
rename_boot_partition
rename_raspi
# rename_pc
close_luks

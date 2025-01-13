#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted

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

create_gpt() {
    echo "Create partition table."
    parted --script $DISK mklabel gpt
    parted --script $DISK mkpart ESP fat32 0% 1GiB
    parted --script $DISK set 1 esp on
}

create_boot_partition() {
    echo "Create boot partition."
    mkfs.fat -F32 -n BOOTTOFRMT $BOOT_PARTITION
    sleep 5
}

create_main_partition() {
    echo "Create main partition."
    parted --script $DISK mkpart primary 1GiB 100%
}

create_luks_partition() {
    echo "Create LUKS partition."
    cryptsetup luksFormat --label cryptroot $ROOT_PARTITION
    cryptsetup open $ROOT_PARTITION $LUKS_NAME
}

create_swap() {
    echo "Create swap partition."
    lvcreate -L 8G $VGROUP -n SWAPTOFRMT
    mkswap -L SWAPTOFRMT /dev/$VGROUP/SWAPTOFRMT
}

create_btrfs() {
    echo "Create btrfs"
    mkfs.btrfs -L mainBtrfsToFormat "$LUKS_PATH" # Creating btrfs partition
    mkdir -p "$ROOT_DIR"/{home,nix,swap}
    mount -t btrfs "$LUKS_PATH" "$ROOT_DIR"
    btrfs subvolume create "$ROOT_DIR"/root # The subvolume for /
    btrfs subvolume create "$ROOT_DIR"/home
    btrfs subvolume create "$ROOT_DIR"/nix
    btrfs subvolume create "$ROOT_DIR"/swap
    umount "$ROOT_DIR"
}
# }

create_f2fs() {
    echo "Create f2fs"
    mkfs.f2fs -l ROOTTOFRMT $LUKS_PATH
}

mount_partitions() {
    echo "Mount partitions."
    sleep 5
    mount -o subvol=root,compress=zstd,noatime "$LUKS_PATH" "$ROOT_DIR"
    mkdir -p "$ROOT_DIR"/{home,nix,swap}
    mount -o subvol=home,compress=zstd,noatime "$LUKS_PATH" "$ROOT_DIR"/home
    mount -o subvol=nix,compress=zstd,noatime "$LUKS_PATH" "$ROOT_DIR"/nix
    mount -o subvol=swap,noatime "$LUKS_PATH" "$ROOT_DIR"/swap
    btrfs filesystem mkswapfile --size 64g --uuid clear /swap/swapfile

    mkdir -p "$BOOT_DIR"
    mount /dev/disk/by-label/BOOTTOFRMT "$BOOT_DIR"
}

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

create_uefi() {
    echo "Create UEFI"
    curl -o /tmp/pi4-uefi.zip -L https://github.com/pftf/RPi4/releases/download/v1.35/RPi4_UEFI_Firmware_v1.35.zip
    unzip /tmp/pi4-uefi.zip -d $BOOT_DIR
    sync
}

create_initrd_keys() {
    mkdir -p $ROOT_DIR/etc/secrets/initrd
    ssh-keygen -t ed25519 -N "" -C "" -f $ROOT_DIR/etc/secrets/initrd/ssh_host_ed25519_key
}

create_ssh_host_keys() {
    host="budget"
    mkdir -p $ROOT_DIR/etc/ssh
    ssh-keygen -t ed25519 -N "" -C "root@$host" -f $ROOT_DIR/etc/ssh/ssh_host_ed25519_key
    ssh-keygen -N "" -C "root@$host" -t rsa -b 4096 -f $ROOT_DIR/etc/ssh/ssh_host_rsa_key
    ssh-keygen -N "" -C "root@$host" -t ecdsa -f $ROOT_DIR/etc/ssh/ssh_host_ecdsa_key
    echo ""
    cat $ROOT_DIR/etc/ssh/ssh_host_ed25519_key.pub
}

create_pi() {
    create_gpt
    create_boot_partition
    create_main_partition
    create_luks_partition
    create_f2fs
    mount_partitions
    create_uefi
    create_initrd_keys
    create_ssh_host_keys
}

create_pc() {
    create_gpt
    create_boot_partition
    create_main_partition
    create_luks_partition
    create_btrfs
    mount_partitions
    create_initrd_keys
    create_ssh_host_keys
}

create_pc

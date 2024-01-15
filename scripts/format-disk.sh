#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted

DISK=/dev/mmcblk0
ROOT_PARTITION="$DISK"p2
ROOT_DIR=/mnt/nixos
BOOT_DIR=/mnt/nixos/boot

echo "Create partition table."
parted --script $DISK mklabel gpt
parted --script $DISK mkpart ESP fat32 0% 1GiB
parted --script $DISK set 1 esp on

echo "Create boot partition."
mkfs.fat -F32 -n BOOTTOFRMT "$DISK"p1
sleep 5

echo "Create main partition."
parted --script $DISK mkpart primary 1GiB 100%

echo "Create LUKS partition."
LUKS_NAME=crypttoformat
LUKS_PATH=/dev/mapper/$LUKS_NAME
cryptsetup luksFormat --label cryptroot $ROOT_PARTITION
cryptsetup open $ROOT_PARTITION $LUKS_NAME

# # PC Setup {
# echo "Create LVM partition."
# VGROUP=grouptoformat
# pvcreate $LUKS_PATH
# vgcreate $VGROUP $LUKS_PATH

# echo "Create swap partition."
# lvcreate -L 8G $VGROUP -n SWAPTOFRMT
# mkswap -L SWAPTOFRMT /dev/$VGROUP/SWAPTOFRMT

# echo "Create ext4"
# lvcreate -l 100%FREE $VGROUP -n ROOTTOFRMT
# mkfs.ext4 -L ROOTTOFRMT "/dev/$VGROUP/ROOTTOFRMT"
# # }

# Raspberry Pi Setup {
echo "Create f2fs"
mkfs.f2fs -l ROOTTOFRMT $LUKS_PATH
# }

echo "Mount partitions."
mkdir -p $ROOT_DIR
mount /dev/disk/by-label/ROOTTOFRMT $ROOT_DIR
mkdir -p $BOOT_DIR
mount /dev/disk/by-label/BOOTTOFRMT $BOOT_DIR

echo "Create UEFI"
curl -o /tmp/pi4-uefi.zip -L https://github.com/pftf/RPi4/releases/download/v1.35/RPi4_UEFI_Firmware_v1.35.zip
unzip /tmp/pi4-uefi.zip -d $BOOT_DIR
sync

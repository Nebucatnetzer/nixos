#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parted

DISK=/dev/sdb
BOOT_PARTITION="$DISK"1
ROOT_PARTITION="$DISK"2
ROOT_DIR=/mnt/nixos
BOOT_DIR=/mnt/nixos/boot
LUKS_NAME=crypttoformat
LUKS_PATH=/dev/mapper/$LUKS_NAME
VGROUP=grouptoformat

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

create_lvm() {
	echo "Create LVM partition."
	pvcreate $LUKS_PATH
	vgcreate $VGROUP $LUKS_PATH
}

create_swap() {
	echo "Create swap partition."
	lvcreate -L 8G $VGROUP -n SWAPTOFRMT
	mkswap -L SWAPTOFRMT /dev/$VGROUP/SWAPTOFRMT
}

create_ext4() {
	echo "Create ext4"
	lvcreate -l 100%FREE $VGROUP -n ROOTTOFRMT
	mkfs.ext4 -L ROOTTOFRMT "/dev/$VGROUP/ROOTTOFRMT"
}
# }

create_f2fs() {
	echo "Create f2fs"
	mkfs.f2fs -l ROOTTOFRMT $LUKS_PATH
}

mount_partitions() {
	echo "Mount partitions."
	mkdir -p $ROOT_DIR
	mount /dev/disk/by-label/ROOTTOFRMT $ROOT_DIR
	mkdir -p $BOOT_DIR
	mount /dev/disk/by-label/BOOTTOFRMT $BOOT_DIR
}

create_uefi() {
	echo "Create UEFI"
	curl -o /tmp/pi4-uefi.zip -L https://github.com/pftf/RPi4/releases/download/v1.35/RPi4_UEFI_Firmware_v1.35.zip
	unzip /tmp/pi4-uefi.zip -d $BOOT_DIR
	sync
}

create_pi() {
	create_gpt
	create_boot_partition
	create_main_partition
	create_luks_partition
	create_f2fs
	mount_partitions
	create_uefi
}

create_pc() {
	create_gpt
	create_boot_partition
	create_main_partition
	create_luks_partition
	create_lvm
	create_swap
	create_ext4
	mount_partitions
}

create_pi

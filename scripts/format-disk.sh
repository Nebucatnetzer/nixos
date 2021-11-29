# Print the drives
parted -l
read -p "Which disk do you want to format?: " drive_path
# Create partition table
parted $drive_path -- mklabel gpt
# Create EFI partition
parted $drive_path -- mkpart ESP fat32 1MiB 512MiB
parted $drive_path -- set 1 esp on
# Create the main partition
parted $drive_path -- mkpart primary 512MiB 100%
 
# Encrypt the main partition
main_partition="${drive_path}2"
cryptsetup luksFormat --type luks1 $main_partition
cryptsetup open $main_partition cryptlvm

# Create the LVM groups
pvcreate /dev/mapper/cryptlvm
vgcreate MainGroup /dev/mapper/cryptlvm
# Create the swap volume
lvcreate -L 8G MainGroup -n swap
# Create the main volume
lvcreate -l 100%FREE MainGroup -n root
 
# Format the main volume with EXT4
mkfs.ext4 -L nixos /dev/MainGroup/root
# Enable swap
mkswap -L swap /dev/MainGroup/swap
# Format the boot partition with FAT32
boot_partition="${drive_path}1"
mkfs.fat -F 32 -n BOOT $boot_partition


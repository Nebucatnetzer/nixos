#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 parted

import getpass
import subprocess


def _run_command(command, user_input=""):
    if user_input:
        result = subprocess.run(
            command, capture_output=True, text=True, check=True, input=user_input
        )
    else:
        result = subprocess.run(command, capture_output=True, text=True, check=True)
    return result


def create_partition_table(disk):
    print("Create partition table.")
    _run_command(["parted", "--script", disk, "mklabel", "gpt"])


def create_boot_partition(disk):
    boot_partition = f"{disk}p1"
    print(f"Create boot partition {boot_partition}.")
    _run_command(
        ["parted", "--script", disk, "mkpart", "ESP", "fat32", "1MiB", "1024MiB"]
    )
    _run_command(["parted", "--script", disk, "set", "1", "esp", "on"])
    _run_command(["mkfs.fat", "-F", "32", "-n", "BOOT", boot_partition])


def create_main_partition(disk):
    print("Create main partition.")
    _run_command(["parted", "--script", disk, "mkpart", "primary", "1024MiB", "100%"])
    return f"{disk}p2"


def _create_main_filesystem():
    _run_command(["lvcreate", "-l", "100%FREE", "MainGroupSd", "-n", "root"])
    _run_command(["mkfs.ext4", "-L", "nixos", "/dev/MainGroupSd/root"])


def _encrypt_disk(partition_path):
    password = getpass.getpass()
    print("Encrypting disk.")
    _run_command(
        ["cryptsetup", "luksFormat", "-q", "--type", "luks2", partition_path],
        user_input=password,
    )
    _run_command(
        ["cryptsetup", "open", partition_path, "cryptlvmsd"], user_input=password
    )


def _setup_lvm(lvm_target):
    print("Set up LVM on {}.".format(lvm_target))
    _run_command(["pvcreate", lvm_target])
    _run_command(["vgcreate", "MainGroupSd", lvm_target])


def create_file_systems(partition):
    print("Creating filesystems.")
    lvm_target = "/dev/mapper/cryptlvmsd"
    _encrypt_disk(partition)
    _setup_lvm(lvm_target)
    _create_main_filesystem()


def main():
    disk_to_format = "/dev/mmcblk0"
    create_partition_table(disk_to_format)
    create_boot_partition(disk_to_format)
    main_partition = create_main_partition(disk_to_format)
    create_file_systems(main_partition)


if __name__ == "__main__":
    main()

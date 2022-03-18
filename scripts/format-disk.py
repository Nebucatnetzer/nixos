#!/usr/bin/env python3
import getpass
import os
import subprocess
import sys
import time


def _run_command(command, user_input=""):
    if user_input:
        result = subprocess.run(command,
                                capture_output=True,
                                text=True,
                                check=True,
                                input=user_input)
    else:
        result = subprocess.run(command,
                                capture_output=True,
                                text=True,
                                check=True)
    return result


def _get_system_memory():
    mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
    mem_gib = mem_bytes / (1024.**3)
    return round(mem_gib)


def _y_n(question):
    answer = input("{} (Y/N): ".format(question))
    if answer.lower() == "y":
        return True
    if answer.lower() == "n":
        return False
    print("Please only answer with Y or N!")
    sys.exit(1)


def read_disks():
    output = _run_command(["lsblk", "-dpno", "name"])
    disks = []
    for disk in output.stdout.splitlines():
        if "loop" in disk:
            continue
        disks.append(disk)
    return disks


def create_menu(disks):
    for position, disk in enumerate(disks):
        print("{}: {}".format(position, disk))


def get_disk_to_format():
    disk_to_format = input("Which disk dou you want to format?: ")
    return int(disk_to_format)


def create_partition_table(disk):
    print("Create partition table.")
    _run_command(["parted", "--script", disk, "mklabel", "gpt"])


def _partition_suffix(disk):
    if "nvmne" in disk:
        return "p"
    if "mmcblk" in disk:
        return "p"
    return ""


def create_partitions(disk):
    print("Create partitions")
    _run_command(["parted", "--script", disk,
                  # UEFI
                  "mkpart", "ESP", "fat32", "1MiB", "512MiB",
                  "set", "1", "esp", "on",
                  # Main
                  "mkpart", "primary", "512MiB", "100%"])


def _create_ext4_filesystem():
    _run_command(["lvcreate", "-l", "100%FREE", "MainGroup", "-n", "root"])
    _run_command(["mkfs.ext4", "-L", "nixos", "/dev/MainGroup/root"])


def _create_f2fs_filesystem(partition):
    _run_command(["mkfs.f2fs", "-L", "nixos", partition])


def _create_swap():
    memory = _get_system_memory()
    print("Create swap partition of {} GiB in size".format(memory))
    _run_command(["lvcreate",
                  "-L",
                  "{}G".format(memory),
                  "MainGroup",
                  "-n",
                  "swap"])
    _run_command(["mkswap", "-L", "swap", "/dev/MainGroup/swap"])


def _encrypt_disk(partition_path):
    password = getpass.getpass()
    print("Encrypting disk.")
    _run_command(["cryptsetup", "luksFormat", "-q",
                  "--type", "luks1", partition_path], user_input=password)
    _run_command(["cryptsetup", "open", partition_path, "cryptlvm"], user_input=password)


def _setup_lvm(lvm_target):
    print("Set up LVM on {}.".format(lvm_target))
    _run_command(["pvcreate", lvm_target])
    _run_command(["vgcreate", "MainGroup", lvm_target])


def mount_partitions():
    print("Mounting partitions.")
    # wait for a second, required because the filesystem needs some time
    time.sleep(1)
    _run_command(["mount", "/dev/disk/by-label/nixos", "/mnt"])
    os.mkdir("/mnt/boot")
    _run_command(["mount", "/dev/disk/by-label/BOOT", "/mnt/boot"])


def create_uefi_filesystem(boot_partition):
    _run_command(["mkfs.fat", "-F", "32", "-n", "BOOT", boot_partition])


def create_x86_filesystems(partition, encryption):
    print("Creating filesystems.")
    if encryption:
        lvm_target = "/dev/mapper/cryptlvm"
        _encrypt_disk(partition)
    else:
        lvm_target = partition
    _setup_lvm(lvm_target)
    _create_swap()
    _create_ext4_filesystem()


def create_aarch64_filesystem(partition):
    print("Creating filesystems.")
    _create_f2fs_filesystem()


def main():
    disks = read_disks()
    create_menu(disks)
    disk_to_format = disks[get_disk_to_format()]
    partition_suffix = _partition_suffix(disk_to_format)
    raspberry = _y_n("Is this a Raspberry Pi?")
    encryption = _y_n("Do you want to encrypt your data?")

    if raspberry:
        boot_partition = "{}{}2".format(disk_to_format, partition_suffix)
        main_partition = "{}{}3".format(disk_to_format, partition_suffix)
        create_partitions(disk_to_format)
        create_uefi_filesystem(boot_partition)
        create_aarch64_filesystem(main_partition)
    else:
        boot_partition = "{}{}1".format(disk_to_format, partition_suffix)
        main_partition = "{}{}2".format(disk_to_format, partition_suffix)
        create_partition_table(disk_to_format)
        create_partitions(disk_to_format)
        create_uefi_filesystem(boot_partition)
        create_x86_filesystems(main_partition, encryption)
    mount_partitions()


if __name__ == "__main__":
    main()

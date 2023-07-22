#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

import getpass
import os
import subprocess
import sys


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
    return ""


def create_boot_partition(disk):
    boot_partition = "{}{}1".format(disk, _partition_suffix(disk))
    print("Create boot partition {}.".format(boot_partition))
    _run_command(["parted", "--script", disk, "mkpart",
                  "ESP", "fat32", "1MiB", "512MiB"])
    _run_command(["parted", "--script", disk, "set", "1", "esp", "on"])
    _run_command(["mkfs.fat", "-F", "32", "-n", "BOOT", boot_partition])


def create_main_partition(disk):
    print("Create main partition.")
    _run_command(["parted", "--script", disk, "mkpart",
                  "primary", "512MiB", "100%"])
    return "{}{}2".format(disk, _partition_suffix(disk))


def _create_main_filesystem():
    _run_command(["lvcreate", "-l", "100%FREE", "MainGroup", "-n", "root"])
    _run_command(["mkfs.ext4", "-L", "nixos", "/dev/MainGroup/root"])


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
    _run_command(["mount", "/dev/MainGroup/root", "/mnt"])
    os.mkdir("/mnt/boot")
    _run_command(["mount", "/dev/disk/by-label/BOOT", "/mnt/boot"])


def create_file_systems(partition, swap, encryption):
    print("Creating filesystems.")
    if encryption:
        lvm_target = "/dev/mapper/cryptlvm"
        _encrypt_disk(partition)
    else:
        lvm_target = partition
    _setup_lvm(lvm_target)
    if swap:
        _create_swap()
    _create_main_filesystem()


def main():
    disks = read_disks()
    create_menu(disks)
    disk_to_format = disks[get_disk_to_format()]
    swap = _y_n("Do you need swap?")
    encryption = _y_n("Do you want to encrypt your data?")
    create_partition_table(disk_to_format)
    create_boot_partition(disk_to_format)
    main_partition = create_main_partition(disk_to_format)
    create_file_systems(main_partition, swap, encryption)
    mount_partitions()


if __name__ == "__main__":
    main()

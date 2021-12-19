#!/usr/bin/env python3
import os
import subprocess
import sys


def _run_command(command):
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
    print("Please only anwser with Y or N!")
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
    for id, disk in enumerate(disks):
        print("{}: {}".format(id, disk))


def get_disk_to_format():
    disk_to_format = input("Which disk dou you want to format?: ")
    return int(disk_to_format)


def _create_partition_table(disk):
    _run_command(["parted", disk, "--", "mklabel", "gpt"])


def _partition_suffix(disk):
    if "nvmne" in disk:
        return "p"
    return ""


def create_boot_partition(disk):
    boot_partition = "{}{}1".format(disk, _partition_suffix(disk))
    print("Create boot partition {}.".format(boot_partition))
    _run_command(["parted", disk, "--", "mkpart",
                  "ESP", "fat32", "1MiB", "512MiB"])
    _run_command(["parted", disk, "--", "set", 1, "esp", "on"])
    _run_command(["mkfs.fat", "-F", 32, "-n", "BOOT", boot_partition])


def _create_main_partition(disk):
    print("Create main partition")
    _run_command(["lvcreate", "-l", "100%FREE", "MainGroup", "-n", "root"])
    _run_command(["mkfs.ext4", "-L", "nixos", "/dev/MainGroup/root"])


def _create_swap_partition(disk):
    memory = _get_system_memory()
    print("Create swap partition of {} GiB in size".format(memory))
    _run_command(["lvcreate",
                  "-L",
                  "{}G".format(memory),
                  "MainGroup",
                  "-n",
                  "swap"])
    _run_command(["mkswap", "-L", "swap", "/dev/MainGroup/swap"])


def _encrypt_disk(partition_path, container_name):
    print("Encrypting disk.")
    _run_command(["cryptsetup", "luksFormat", "-q"
                 "--type", "luks1", partition_path])
    _run_command(["cryptsetup", "open", partition_path, "cryptlvm"])


def _setup_lvm(lvm_target):
    print("Set up LVM.")
    _run_command(["pvcreate", lvm_target])
    _run_command(["vgcreate", "MainGroup", lvm_target])


def mount_partitions():
    _run_command(["mount", "/dev/MainGroup/root", "/mnt"])
    os.mkdir("/mnt/boot")
    _run_command(["mount", "/dev/disk/by-label/BOOT", "/mnt/boot"])


def format_disk(disk, swap_partition, encryption):
    print("Formatting disk: {}.".format(disk))
    _run_command(["parted", disk, "--", "mkpart", "primary", "512MiB", "100%"])
    main_partition_path = "{}{}2".format(disk, _partition_suffix(disk))
    lvm_target = ""
    if encryption:
        lvm_target = "/dev/mapper/cryptlvm"
        _encrypt_disk(main_partition_path, lvm_target)
    else:
        lvm_target = main_partition_path
    _setup_lvm(lvm_target)
    if swap_partition:
        _create_swap_partition(disk)
    _create_main_partition(disk)


def main():
    disks = read_disks()
    create_menu(disks)
    disk_to_format = disks[get_disk_to_format()]
    swap_partition = _y_n("Do you need a swap partition?")
    encryption = _y_n("Do you want to encrypt your data?")
    create_boot_partition(disk_to_format)
    format_disk(disk_to_format, swap_partition, encryption)
    mount_partitions()


if __name__ == "__main__":
    main()

#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 parted

import getpass
import os
import sys
import subprocess
import zipfile

from time import sleep

from urllib.request import urlretrieve


def _run_command(command, user_input=""):
    if user_input:
        result = subprocess.run(
            command, capture_output=True, text=True, check=True, input=user_input
        )
    else:
        result = subprocess.run(command, capture_output=True, text=True, check=True)
    return result


def _get_system_memory():
    mem_bytes = os.sysconf("SC_PAGE_SIZE") * os.sysconf("SC_PHYS_PAGES")
    mem_gib = mem_bytes / (1024.0**3)
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
    sleep(5)


def _partition_suffix(disk):
    prefixes = ["nvmne", "mmc"]
    if any(prefix in disk for prefix in prefixes):
        return "p"
    return ""


def _get_uefi():
    filename = "RPi4_UEFI_Firmware_v1.35.zip"
    url = f"https://github.com/pftf/RPi4/releases/download/v1.35/{filename}"
    retrieved_file, _ = urlretrieve(url, f"/tmp/{filename}")
    return retrieved_file


def _extract_uefi(path_to_zip, partition):
    with zipfile.ZipFile(path_to_zip, "r") as zip_ref:
        zip_ref.extractall(partition)


def _create_uefi():
    print("Create UEFI partition.")
    filename = _get_uefi()
    _extract_uefi(filename, "/mnt/nixos/boot")


def create_boot_partition(disk):
    boot_partition = "{}{}1".format(disk, _partition_suffix(disk))
    print("Create boot partition {}.".format(boot_partition))
    _run_command(["parted", "--script", disk, "mkpart", "ESP", "fat32", "0%", "1GiB"])
    _run_command(["parted", "--script", disk, "set", "1", "esp", "on"])
    _run_command(["mkfs.fat", "-F", "32", "-n", "BOOTTOFRMT", boot_partition])
    sleep(5)


def create_main_partition(disk):
    print("Create main partition.")
    _run_command(["parted", "--script", disk, "mkpart", "primary", "1GiB", "100%"])
    return "{}{}2".format(disk, _partition_suffix(disk))


def _create_ext4():
    _run_command(["lvcreate", "-l", "100%FREE", "grouptoformat", "-n", "ROOTTOFRMT"])
    _run_command(["mkfs.ext4", "-L", "ROOTTOFRMT", "/dev/grouptoformat/ROOTTOFRMT"])


def _create_f2fs():
    _run_command(["mkfs.f2fs", "-l", "ROOTTOFRMT", "/dev/mapper/crypttoformat"])


def _create_swap():
    memory = _get_system_memory()
    print("Create swap partition of {} GiB in size".format(memory))
    _run_command(
        ["lvcreate", "-L", "{}G".format(memory), "grouptoformat", "-n", "SWAPTOFRMT"]
    )
    _run_command(["mkswap", "-L", "SWAPTOFRMT", "/dev/grouptoformat/SWAPTOFRMT"])


def _encrypt_disk(partition_path):
    password = getpass.getpass()
    print("Encrypting disk.")
    _run_command(
        ["cryptsetup", "luksFormat", "-q", partition_path],
        user_input=password,
    )
    _run_command(
        ["cryptsetup", "open", partition_path, "crypttoformat"], user_input=password
    )
    return "/dev/mapper/crypttoformat"


def _setup_lvm(lvm_target):
    print("Set up LVM on {}.".format(lvm_target))
    _run_command(["pvcreate", lvm_target])
    _run_command(["vgcreate", "grouptoformat", lvm_target])


def mount_partitions(
    root_src="/dev/disk/by-label/ROOTTOFRMT",
    boot_src="/dev/disk/by-label/BOOTTOFRMT",
    root_target="/mnt/nixos",
    boot_target="/mnt/nixos/boot",
):
    print("Mounting partitions.")
    sleep(5)
    os.makedirs(root_target, exist_ok=True)
    _run_command(["mount", root_src, root_target])
    os.makedirs(boot_target, exist_ok=True)
    _run_command(["mount", boot_src, boot_target])


def create_pc(partition, swap, encryption):
    print("Creating filesystems.")
    if encryption:
        lvm_target = "/dev/mapper/crypttoformat"
        _encrypt_disk(partition)
    else:
        lvm_target = partition
    _setup_lvm(lvm_target)
    if swap:
        _create_swap()
    _create_ext4()


def create_raspi(main_partition):
    print("Create filesystems.")
    _encrypt_disk(main_partition)
    _create_f2fs()
    mount_partitions()
    _create_uefi()


def main():
    disks = read_disks()
    create_menu(disks)
    disk_to_format = disks[get_disk_to_format()]
    raspi = _y_n("Do you want to setup a Raspi?")
    create_partition_table(disk_to_format)
    create_boot_partition(disk_to_format)
    main_partition = create_main_partition(disk_to_format)
    if raspi:
        create_raspi(main_partition)
    else:
        swap = _y_n("Do you need swap?")
        encryption = _y_n("Do you need encryption?")
        create_pc(main_partition, swap, encryption)


if __name__ == "__main__":
    main()

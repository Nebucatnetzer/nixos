#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 parted

import getpass
import os
import subprocess


def _run_command(command, user_input=""):
    if user_input:
        result = subprocess.run(
            command, capture_output=True, text=True, check=True, input=user_input
        )
    else:
        result = subprocess.run(command, capture_output=True, text=True, check=True)
    return result


def _partition_suffix(disk):
    if ("nvmne" or "mmc") in disk:
        return "p"
    return ""


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


def create_main_partition(disk):
    print("Create main partition.")
    _run_command(["parted", "--script", disk, "mkpart", "primary", "1GiB", "100%"])
    return f"{disk}{_partition_suffix(disk)}2"


def _create_main_filesystem():
    _run_command(
        [
            "mkfs.f2fs",
            "-l",
            "NixosSd",
            "-O",
            "extra_attr,inode_checksum,sb_checksum,compression",
            "/dev/mapper/cryptsd",
        ]
    )


def _encrypt_disk(partition_path):
    password = getpass.getpass()
    print("Encrypting disk.")
    _run_command(
        ["cryptsetup", "luksFormat", "-q", partition_path],
        user_input=password,
    )
    _run_command(["cryptsetup", "open", partition_path, "cryptsd"], user_input=password)


def create_file_systems(partition):
    print("Creating filesystems.")
    _encrypt_disk(partition)
    _create_main_filesystem()


def mount_partitions():
    print("Mounting partitions.")
    _run_command(["mount", "/dev/disk/by-label/NixosSd", "/mnt"])
    os.mkdir("/mnt/boot")
    _run_command(["mount", "/dev/disk/by-label/SdBoot", "/mnt/boot"])


def main():
    disks = read_disks()
    create_menu(disks)
    disk_to_format = disks[get_disk_to_format()]
    main_partition = create_main_partition(disk_to_format)
    create_file_systems(main_partition)
    mount_partitions()


if __name__ == "__main__":
    main()

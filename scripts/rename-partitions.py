#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 parted

import subprocess
import sys
from time import sleep


def _run_command(command, user_input=""):
    if user_input:
        result = subprocess.run(
            command, capture_output=True, text=True, check=True, input=user_input
        )
    else:
        result = subprocess.run(command, capture_output=True, text=True, check=True)
    return result


def _y_n(question):
    answer = input("{} (Y/N): ".format(question))
    if answer.lower() == "y":
        return True
    if answer.lower() == "n":
        return False
    print("Please only answer with Y or N!")
    sys.exit(1)


def rename_boot_partition():
    print("Rename boot partition.")
    _run_command(["fatlabel", "/dev/disk/by-label/BOOTTOFRMT", "BOOT"])


def _rename_ext4():
    print("Rename ext4 partition.")
    _run_command(["e2label", "/dev/MainGroup/roottoformat", "root"])


def _rename_f2fs():
    print("Rename f2fs partition.")
    _run_command(["f2fslabel", "/dev/disk/by-label/ROOTTOFRMT", "root"])


def _rename_swap():
    print("Rename swap partition.")
    _run_command(["swaplabel", "-L", "swap", "/dev/GroupToFormat/swaptoformat"])


def _rename_lvm():
    print("Rename LVM")
    _run_command(["lvrename", "GroupToFormat", "roottoformat", "root"])
    _run_command(["vgrename", "GroupToFormat", "MainGroup"])


def unmount_partitions():
    print("Unmounting partitions.")
    _run_command(["umount", "/mnt/nixos/boot"])
    _run_command(["umount", "/mnt/nixos"])
    sleep(3)


def close_luks():
    _run_command(["cryptsetup", "close", "crypttoformat"])


def rename_pc(swap):
    if swap:
        _rename_swap()
    _rename_ext4()
    _rename_lvm()


def rename_raspi():
    _rename_f2fs()


def main():
    raspi = _y_n("Do we rename a Raspberry Pi?")
    unmount_partitions()
    sleep(5)
    rename_boot_partition()
    if raspi:
        rename_raspi()
    else:
        swap = _y_n("Do you have swap?")
        rename_pc(swap)
    close_luks()


if __name__ == "__main__":
    main()

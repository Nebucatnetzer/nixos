#!/usr/bin/env python3
import subprocess


def _run_command(command):
    result = subprocess.run(command,
                            capture_output=True,
                            text=True,
                            check=True)
    return result


def _read_disks():
    output = _run_command(["lsblk", "-dpno", "name"])
    disks = []
    for disk in output.stdout.splitlines():
        if "loop" in disk:
            continue
        disks.append(disk)
    return disks


def _create_menu(disks):
    for id, disk in enumerate(disks):
        print("{}: {}".format(id, disk))


def _get_disk_to_format():
    disk_to_format = input("Which disk dou you want to format?: ")
    return disk_to_format


def _format_disk(disk_to_format):
    print("Formatting disk: {}.".format(disk_to_format))
    pass


def _encrypt_disk():
    print("Encrypting disk.")
    pass


def main():
    disks = _read_disks()
    _create_menu(disks)


if __name__ == "__main__":
    main()

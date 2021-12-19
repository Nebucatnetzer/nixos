#!/usr/bin/env python3
import os
import subprocess


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


def format_disk(disk_to_format):
    print("Formatting disk: {}.".format(disk_to_format))
    pass


def encrypt_disk():
    print("Encrypting disk.")
    pass


def main():
    disks = read_disks()
    create_menu(disks)
    disk_to_format = disks[get_disk_to_format()]
    format_disk(disk_to_format)
    print(_get_system_memory())


if __name__ == "__main__":
    main()

{
  lib,
  parted,
  btrfs-progs,
  cryptsetup,
  e2fsprogs,
  unzip,
  wget,
  writeShellApplication,
}:
writeShellApplication {
  name = "format-disk";
  runtimeInputs = [
    parted
    btrfs-progs
    cryptsetup
    e2fsprogs
    unzip
    wget
  ];
  meta = {
    description = "Format disk with LUKS, btrfs subvolumes, and UEFI";
    license = lib.licenses.gpl3Plus;
    mainProgram = "format-disk";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./format_disk.sh;
}

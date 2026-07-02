{
  lib,
  btrfs-progs,
  cryptsetup,
  util-linux,
  writeShellApplication,
}:
writeShellApplication {
  name = "unmount-btrfs-partitions";
  runtimeInputs = [
    btrfs-progs
    cryptsetup
    util-linux
  ];
  meta = {
    description = "Unmount btrfs partitions safely";
    license = lib.licenses.gpl3Plus;
    mainProgram = "unmount-btrfs-partitions";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./unmount_btrfs_partitions.sh;
}

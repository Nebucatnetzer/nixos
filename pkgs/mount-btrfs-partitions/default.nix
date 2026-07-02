{
  lib,
  btrfs-progs,
  cryptsetup,
  util-linux,
  writeShellApplication,
}:
writeShellApplication {
  name = "mount-btrfs-partitions";
  runtimeInputs = [
    btrfs-progs
    cryptsetup
    util-linux
  ];
  meta = {
    description = "Mount btrfs partitions for recovery";
    license = lib.licenses.gpl3Plus;
    mainProgram = "mount-btrfs-partitions";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./mount_btrfs_partitions.sh;
}

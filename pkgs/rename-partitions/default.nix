{
  lib,
  parted,
  btrfs-progs,
  cryptsetup,
  util-linux,
  writeShellApplication,
}:
writeShellApplication {
  name = "rename-partitions";
  runtimeInputs = [
    parted
    btrfs-progs
    cryptsetup
    util-linux
  ];
  meta = {
    description = "Rename disk partitions and labels";
    license = lib.licenses.gpl3Plus;
    mainProgram = "rename-partitions";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./rename_partitions.sh;
}

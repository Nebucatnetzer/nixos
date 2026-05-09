{
  btrfsLabel ? "mainBtrfs",
  bootLabel ? "BOOT",
}:
{ ... }:
let
  commonBtrfsOptions = import ./common_options.nix;
in
{
  fileSystems."/" = {
    fsType = "btrfs";
    label = btrfsLabel;
    neededForBoot = true;
    options = [ "subvol=root" ] ++ commonBtrfsOptions;
  };
  fileSystems."/home" = {
    fsType = "btrfs";
    label = btrfsLabel;
    neededForBoot = true;
    options = [ "subvol=home" ] ++ commonBtrfsOptions;
  };
  fileSystems."/nix" = {
    fsType = "btrfs";
    label = btrfsLabel;
    neededForBoot = true;
    options = [ "subvol=nix" ] ++ commonBtrfsOptions;
  };
  fileSystems."/swap" = {
    fsType = "btrfs";
    label = btrfsLabel;
    options = [
      "compress=no"
      "noatime"
      "nodatacow"
      "nodatasum"
      "ssd"
      "subvol=swap"
    ];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/${bootLabel}";
    fsType = "vfat";
  };
  swapDevices = [ { device = "/swap/swapfile"; } ];
}

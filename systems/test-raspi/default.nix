{ hostname }:
{ inputs, ... }:
let
  btrfsAuxModule = import "${inputs.self}/modules/hardware/btrfs/aux.nix";
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/programs/nix-direnv"
    "${inputs.self}/modules/services/docker"
    (btrfsAuxModule { mountPath = "/mnt/test-usb"; })
    raspi4Configs.diskLayouts.singleSdCard
    (raspi4Configs.ethernet {
      inherit hostname;
      ip = "10.7.89.40";
    })
  ];

  fileSystems."/mnt/test-usb" = {
    fsType = "btrfs";
    device = "/dev/disk/by-partlabel/disk-testUsbStick-btrfs";
    neededForBoot = false;
    options = [ ] ++ commonBtrfsOptions;
  };

  nixpkgs.hostPlatform = "aarch64-linux";
}

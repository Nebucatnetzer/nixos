{ hostname }:
{ inputs, ... }:
let
  btrfsModule = import "${inputs.self}/modules/hardware/btrfs";
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.30";
  };
  resticServer = import "${inputs.self}/modules/services/restic-server" { };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/programs/restic-management"
    (btrfsModule {
      filesystemName = "restic-ssd";
      beesSpec = "/var/lib/restic-server";
    })
    raspi4Configs.diskLayouts.singleSdCard
    raspiEthernet
    resticServer
  ];

  fileSystems."/var/lib/restic-server" = {
    fsType = "btrfs";
    label = "resticSSD";
    neededForBoot = false;
    options = [
      "subvol=restic-repo"
    ]
    ++ commonBtrfsOptions;
  };
}

{ hostname }:
{ inputs, ... }:
let
  btrfsModule = import "${inputs.self}/modules/hardware/btrfs";
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.30";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/programs/restic-management"
    (btrfsModule { btrfsLabel = "resticSSD"; })
    raspiEthernet
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

  services.az-restic-server.enable = true;
}

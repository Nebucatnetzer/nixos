{ ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ../../modules/common.nix
      ./hardware-configuration.nix
    ];

  networking = {
    interfaces.enp0s18.useDHCP = true;
  };
}


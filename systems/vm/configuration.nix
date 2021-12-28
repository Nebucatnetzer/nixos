{ ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ../../modules/desktop.nix
      ./hardware-configuration.nix
    ];

  networking = {
    hostName = "nixos-vm";
    interfaces.enp0s3.useDHCP = true;
  };

  environment.variables = {
    ZWEILI_HARDWARE = "vm";
  };

}


{ ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ../../modules/common.nix
      ../../modules/docker.nix
      ../../modules/code-server
      ./hardware-configuration.nix
    ];

  networking = {
    hostName = "nixos-test-vm";
    interfaces.enp0s18.useDHCP = true;
  };
}


{ ... }:
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  networking = {
    hostName = "nixos-test-vm";
    interfaces.ens18.useDHCP = true;
  };
}


{ ... }:
{
  imports =
    [
      ../../modules/desktop.nix
      ../../hardware/bluetooth
      ./hardware-configuration.nix
    ];

  networking.hostName = "gwyn"; # Define your hostname.
  virtualisation.virtualbox.host.enable = true;
}


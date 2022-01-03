{ ... }:
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  networking.hostName = "gwyn"; # Define your hostname.
  virtualisation.virtualbox.host.enable = true;
}


{ ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ../../modules/desktop.nix
      ../../hardware/bluetooth
      ./hardware-configuration.nix
    ];

  networking.hostName = "staubfinger"; # Define your hostname.

  virtualisation.virtualbox.host.enable = true;

  environment.variables = {
    ZWEILI_HARDWARE = "asus";
  };

}


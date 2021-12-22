# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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


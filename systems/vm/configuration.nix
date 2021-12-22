# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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


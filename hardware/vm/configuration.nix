# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../common/desktop.nix
      ./hardware-configuration.nix
    ];

  networking.interfaces.enp0s3.useDHCP = true;

  environment.variables = {
    ZWEILI_HARDWARE = "vm";
  };

}


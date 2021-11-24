# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../common/desktop.nix
      ../bluetooth.nix
      ./hardware-configuration.nix
    ];

  networking.hostName = "gwyn"; # Define your hostname.
  virtualisation.virtualbox.host.enable = true;

  environment.variables = {
    ZWEILI_HARDWARE = "xps5530";
  };

}


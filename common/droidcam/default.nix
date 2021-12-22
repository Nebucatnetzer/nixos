# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, services, ... }:
{
  programs.droidcam.enable = true;
  # required for USB connection
  services.usbmuxd.enable = true;
}


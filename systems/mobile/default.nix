{ hostname }:
{ inputs, lib, pkgs, ... }: {
  hardware = {
    az-raspi4-usb = {
      enable = true;
      hostname = hostname;
      ip = "10.213.0.1";
    };
  };

  programs = { az-nix-direnv.enable = true; };
}

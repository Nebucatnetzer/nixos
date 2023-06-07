{ hostname }: { inputs, lib, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4/raspi-usb.nix"
  ];
  hardware = {
    az-raspi4-usb = {
      enable = true;
      hostname = hostname;
      ip = "10.213.0.1";
    };
  };

  programs = {
    az-nix-direnv.enable = true;
    az-tmux.enable = true;
  };
}

{ hostname }:
{ inputs, ... }:
let

  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.113";
  };
in
{
  hardware = {
    az-raspi4-usb = {
      enable = true;
      hostname = hostname;
      ip = "10.213.0.1";
    };
  };

  profiles.az-server.enable = true;
  programs = {
    az-nix-direnv.enable = true;
  };
}

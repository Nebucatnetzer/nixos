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
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.10";
    };
  };

  services.az-docker.enable = true;
}

{ hostname }:
{ inputs, ... }:
let
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.40";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    raspiEthernet
  ];
  services.az-docker.enable = true;
  programs = {
    az-nix-direnv.enable = true;
  };
}

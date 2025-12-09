{ hostname }:
{ inputs, ... }:
let
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.30";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    raspiEthernet
  ];
  services.az-restic-server.enable = true;
}

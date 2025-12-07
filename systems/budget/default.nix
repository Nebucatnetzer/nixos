{ hostname }:
{ inputs, ... }:
let
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.113";
  };
in
{
  imports = [
    raspiEthernet
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/actualbudget"
    "${inputs.self}/modules/services/eactual"
  ];
}

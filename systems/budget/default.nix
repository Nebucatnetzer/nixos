{ hostname }:
{ inputs, ... }:
let
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.113";
  };
in
{
  imports = [
    raspi4Configs.diskLayouts.singleSdCard
    raspiEthernet
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/actualbudget"
    "${inputs.self}/modules/services/eactual"
  ];
}

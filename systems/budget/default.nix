{ hostname }:
{ inputs, ... }:
let
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/actualbudget"
    "${inputs.self}/modules/services/eactual"
    raspi4Configs.diskLayouts.singleSdCard
    (raspi4Configs.ethernet {
      inherit hostname;
      ip = "10.7.89.113";
    })
  ];
}

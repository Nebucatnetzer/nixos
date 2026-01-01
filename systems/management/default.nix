{ hostname }:
{ inputs, ... }:
let
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.150";
  };
  resticClientServer = import "${inputs.self}/modules/services/restic-client-server";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/binary-cache-server"
    raspi4Configs.diskLayouts.singleSdCard
    raspiEthernet
    (resticClientServer {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    })
  ];
}

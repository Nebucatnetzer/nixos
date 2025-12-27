{ hostname }:
{ inputs, ... }:
let
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
    raspiEthernet
    (resticClientServer {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    })
  ];
}

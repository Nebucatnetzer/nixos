{ hostname }:
{ inputs, pkgs, ... }:
let
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.150";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/binary-cache-server"
    raspiEthernet
  ];
  services = {
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    };
  };
}

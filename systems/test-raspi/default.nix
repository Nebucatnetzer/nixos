{ hostname }:
{ inputs, ... }:
let
  raspiUsb = import "${inputs.self}/modules/hardware/raspi4/raspi-usb.nix" {
    inherit hostname;
    ip = "10.7.89.40";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/programs/nix-direnv"
    "${inputs.self}/modules/services/docker"
    raspiUsb
  ];

  nixpkgs.hostPlatform = "aarch64-linux";
}

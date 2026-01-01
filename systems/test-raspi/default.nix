{ hostname }:
{ inputs, ... }:
let
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/programs/nix-direnv"
    "${inputs.self}/modules/services/docker"
    raspi4Configs.diskLayouts.singleSdCard
    (raspi4Configs.ethernet {
      inherit hostname;
      ip = "10.7.89.40";
    })
  ];

  nixpkgs.hostPlatform = "aarch64-linux";
}

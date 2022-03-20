{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      hostname = "raspi-test";
      ip = "10.7.89.99";
      inherit inputs pkgs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "11:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/code-server"
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/nix-direnv"
    "${inputs.self}/modules/raspi-haproxy"
  ];
}

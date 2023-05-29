{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.40";
      inherit hostname;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/nix-direnv"
    "${inputs.self}/modules/tmux"
  ];
}

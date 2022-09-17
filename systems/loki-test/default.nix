{ hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/pihole" # needs to be limited to lan interface
    "${inputs.self}/modules/router"
    "${inputs.self}/modules/tmux"
    "${inputs.self}/modules/unbound" # needs to be limited to lan interface
  ];
}

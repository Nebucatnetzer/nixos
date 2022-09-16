{ hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/router"
    "${inputs.self}/modules/tmux"
  ];
}

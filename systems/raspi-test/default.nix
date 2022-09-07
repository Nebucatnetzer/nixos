{ hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit hostname inputs pkgs;
    })
    (import "${inputs.self}/modules/nginx-acme" {
      domain = "test.2li.ch";
      inherit inputs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/tmux"
  ];
}

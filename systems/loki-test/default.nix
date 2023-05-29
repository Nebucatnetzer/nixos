{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname;
    })
    "${inputs.self}/modules/tmux"
  ];
  services.az-docker.enable = true;
}

{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname;
    })
  ];
  services.az-docker.enable = true;
  programs.az-tmux.enable = true;
}

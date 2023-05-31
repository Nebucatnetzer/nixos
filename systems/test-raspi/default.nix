{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.40";
      inherit hostname;
    })
  ];
  services.az-docker.enable = true;
  programs = {
    az-nix-direnv.enable = true;
    az-tmux = true;
  };
}

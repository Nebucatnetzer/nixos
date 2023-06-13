{ hostname }: { inputs, pkgs, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.40";
    };
  };

  services.az-docker.enable = true;
  programs = {
    az-nix-direnv.enable = true;
    az-tmux.enable = true;
  };
}

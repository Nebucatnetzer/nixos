{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4"
  ];
  hardware = {
    az-raspi4 = {
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

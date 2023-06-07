{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix"
  ];
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.10";
    };
  };

  services.az-docker.enable = true;
  programs.az-tmux.enable = true;
}

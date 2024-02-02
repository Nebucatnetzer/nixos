{ hostname }:
{ inputs, pkgs, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.10";
    };
  };

  services.az-docker.enable = true;
}

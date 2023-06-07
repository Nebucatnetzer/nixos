{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4"
  ];
  hardware = {
    az-raspi4 = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.30";
    };
  };

  services = {
    az-restic-server.enable = true;
  };
}

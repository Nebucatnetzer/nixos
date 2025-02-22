{ hostname }:
{ ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.30";
    };
  };
  profiles.az-server.enable = true;
  services = {
    az-restic-server.enable = true;
  };
}

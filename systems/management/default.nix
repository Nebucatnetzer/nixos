{ hostname }:
{ ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.150";
    };
  };

  # Features
  profiles.az-server.enable = true;
  services = {
    az-binary-cache-server.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    };
  };
}

{ hostname }: { inputs, pkgs, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.123";
    };
  };

  services = {
    az-mailserver.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      time = "01:00";
    };
    az-roundcube.enable = true;
  };
}


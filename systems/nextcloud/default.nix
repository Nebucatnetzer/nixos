{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4"
  ];
  hardware = {
    az-raspi4 = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.103";
    };
  };


  services = {
    az-nextcloud = {
      enable = true;
      domain = "nextcloud.2li.ch";
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = "/home/andreas";
      time = "01:30";
    };
  };
}

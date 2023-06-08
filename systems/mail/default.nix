{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix"
  ];
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.123";
    };
  };

  services = {
    az-mailserver.enable = true;
    az-nginx-proxy = {
      enable = true;
      domain = "mail.zweili.org";
    };
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      time = "01:00";
    };
  };
}


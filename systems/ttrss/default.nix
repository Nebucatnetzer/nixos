{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix"
  ];
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.115";
    };
  };

  services = {
    az-nginx-proxy = {
      enable = true;
      domain = "rss-bridge.2li.ch";
      port = 8082;
    };
    az-restic-client-server-postgres = {
      enable = true;
      path = "/var/lib/ttrss";
      tag = "tt-rss";
      time = "23:00";
    };
    az-rss-bridge.enable = true;
    az-ttrss-postgres = {
      enable = true;
      domain = "ttrss.2li.ch";
    };
  };
}

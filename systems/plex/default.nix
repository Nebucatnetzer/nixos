{ hostname }:
{ inputs, pkgs, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.112";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-media-share.enable = true;
    az-plex.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/var/lib/plex";
      tag = "plex";
      time = "01:30";
    };
  };
}

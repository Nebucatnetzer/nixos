{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix"
  ];
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.112";
    };
  };

  services = {
    az-media-share.enable = true;
    az-plex.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/var/lib/plex";
      tag = "plex";
      time = "02:30";
    };
  };
}

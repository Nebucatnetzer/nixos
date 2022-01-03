{ ... }:
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  networking.hostName = "staubfinger";
  virtualisation.virtualbox.host.enable = true;
}


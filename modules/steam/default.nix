{ ... }:
{
  programs.steam.enable = true;
  hardware.steam-hardware.enable = true;
  networking.firewall = {
    allowedTCPPorts = [ 27036 ];
    allowedUDPPorts = [ 27031 ];
  };

}

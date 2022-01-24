{ ... }:
{
  networking = {
    enableIPv6 = false;
    firewall.allowedTCPPorts = [ 32400 ];
  };
}

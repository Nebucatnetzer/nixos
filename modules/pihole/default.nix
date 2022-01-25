{ ... }:
{
  networking = {
    firewall.allowedTCPPorts = [
      53 # DNS
      67 # DHCP
      80 # Web Interface
    ];
    firewall.allowedUDPPorts = [
      53 # DNS
      67 # DHCP
    ];
  };
}

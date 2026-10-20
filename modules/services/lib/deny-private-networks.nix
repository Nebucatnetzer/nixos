# systemd serviceConfig for public web apps on gwyn. A compromised app cannot reach the
# LAN or the WireGuard network, but it can still reach the internet.
# systemd prefers allow over deny, so loopback (Pi-hole DNS, local backends) still works.
# The filter also drops incoming connections from these networks, so do not use it on
# a service that LAN or VPN clients connect to directly, like nginx.
{
  IPAddressAllow = [ "localhost" ];
  IPAddressDeny = [
    "10.0.0.0/8"
    "169.254.0.0/16"
    "172.16.0.0/12"
    "192.168.0.0/16"
  ];
}

{ pkgs, ... }:
{
  # Not programs.kdeconnect, because it opens the ports on every interface.
  # Only the trusted WireGuard peers may connect.
  environment.systemPackages = [ pkgs.kdePackages.kdeconnect-kde ];
  networking.firewall.interfaces.wg0 = rec {
    allowedTCPPortRanges = [
      {
        from = 1714;
        to = 1764;
      }
    ];
    allowedUDPPortRanges = allowedTCPPortRanges;
  };
}

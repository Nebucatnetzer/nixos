{ ... }:
{
  networking = {
    firewall.allowedTCPPorts = [
      32400 # Web Interface/ Remote Access
    ];
    firewall.allowedUDPPorts = [
      1900 # DLNA
      5353 # Bonjour/Avahi
      32410 # GDM network discovery
      32412 # GDM network discovery
      32413 # GDM network discovery
      32414 # GDM network discovery
      32469 # Plex DLNA Server
    ];
  };
}

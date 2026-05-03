{ lib, ... }:
{
  # Disable timesyncd to avoid conflicts
  services.ntp.enable = false;
  services.timesyncd.enable = lib.mkForce false;

  services.chrony = {
    enable = true;
    extraConfig = ''
      allow 10.7.89.0/24
      allow 10.7.91.0/24
      allow 10.70.89.0/24
    '';
  };

  # Open the firewall for NTP traffic
  networking.firewall.allowedUDPPorts = [ 123 ];
}

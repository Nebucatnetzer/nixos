{ ... }:
{
  services.nomad = {
    enable = true;
    settings = {
      server = {
        enabled = true;
      };
    };
    networking.firewall.allowedTCPPorts = [ 4646 ];
  }

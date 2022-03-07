{ ... }:
{
  services.nomad = {
    enable = true;
    settings = {
      client = {
        enabled = true;
      };
    };
    enableDocker = true;
    networking.firewall.allowedTCPPorts = [ 4646 ];
  }


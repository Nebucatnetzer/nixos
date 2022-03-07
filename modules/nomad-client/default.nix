{ ... }:
{
  services.nomad = {
    enable = true;
    settings = {
      client = {
        enabled = true;
        servers = [ "10.7.89.140:4647" ];
      };
    };
    enableDocker = true;
  };
  networking.firewall = {
    allowedTCPPorts = [ 4646 4647 4648 ];
    allowedUDPPorts = [ 4648 ];
  };
}
